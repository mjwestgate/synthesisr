#' Parse bibliographic text in a variety of formats
#'
#' @description Text in standard formats - such as imported via
#' `base::readLines()` - can be parsed using a variety of standard formats. Use
#' `detect_parser()` to determine which is the most appropriate parser for your
#' situation. Note that `parse_tsv()` and `parse_csv()` are maintained for
#' backwards compatability only; within `read_ref` these have been replaced
#' by `vroom::vroom()`.
#' @param x A character vector containing bibliographic information in ris
#' format.
#' @return Returns an object of class `bibliography` (ris, bib, or pubmed
#' formats) or `data.frame` (csv or tsv).
#' @example inst/examples/parse_.R
#' @rdname parse_
#' @export
parse_pubmed <- function(x){

  x <- prep_ris(x, detect_delimiter(x), type = "pubmed")

  x_merge <- merge(x,
                   synthesisr::code_lookup[
                     synthesisr::code_lookup$ris_pubmed,
                     c("code", "order", "field")
                   ],
                   by.x = "ris",
                   by.y = "code",
                   all.x = TRUE,
                   all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$row_order), ]

  # find a way to store missing .bib data rather than discard
  if(any(is.na(x_merge$field))){
    rows_tr <- which(is.na(x_merge$field))
    x_merge$field[rows_tr] <- x_merge$ris[rows_tr]

    # ensure all headings have an order
    if(all(is.na(x_merge$order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$order, na.rm = TRUE)
    }
    x_merge$order[rows_tr] <- as.numeric(as.factor(x_merge$ris[rows_tr])) + start_val
  }

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("field", "text", "order")], x_merge$ref)
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field)
    if(any(names(result) == "abstract")){
      result$abstract <- paste(result$abstract, collapse = " ")
    }
    if(any(names(result) == "address")){
      result$address <- strsplit(
        paste(result$address, collapse = " "),
        "\\.\\s"
      )[[1]]
    }
    if(any(names(result) == "title")){
      if(length(result$title) > 1){
        result$title <- paste(result$title, collapse = " ")
      }
    }
    if(any(names(result) == "term_other")){
      names(result)[which(names(result) == "term_other")] <- "keywords"
    }
    if(any(names(result) == "date_published")){
      result$year <- substr(result$date_published, start = 1, stop = 4)
    }
    if(any(names(result) == "article_id")){
      doi_check <- grepl("doi", result$article_id)
      if(any(doi_check)){
        result$doi <- strsplit(result$article_id[which(doi_check)], " ")[[1]][1]
      }
    }

    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    return(result[result_order])
  })

  names(x_final) <- unlist(lapply(x_final, function(a){a$pubmed_id}))
  class(x_final) <- "bibliography"
  return(x_final)
}
