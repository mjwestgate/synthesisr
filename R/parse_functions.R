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


#' @rdname parse_
#' @param tag_naming What format are ris tags in? Defaults to `"best_guess"` See
#' `read_refs()` for a list of accepted arguments.
#' @export
parse_ris <- function(x, tag_naming = "best_guess"){

  x <- prep_ris(x, detect_delimiter(x), type = "generic")

  # create the appropriate lookup file for the specified tag
  if(inherits(tag_naming, "data.frame")){
    if(!any(colnames(tag_naming) == "order")){
      tag_naming$order <- seq_len(nrow(tag_naming))
    }
    code_lookup_thisfile <- tag_naming
  }else{
    if(tag_naming == "none"){
      ris_vals <- unique(x$ris)
      code_lookup_thisfile <- data.frame(
        code = ris_vals,
        field = ris_vals,
        order = seq_along(ris_vals),
        stringsAsFactors = FALSE
      )
    }else if(tag_naming == "best_guess"){
      code_lookup_thisfile <- detect_lookup(tags = unique(x$ris))
    }else if(any(c("wos", "scopus", "ovid", "asp", "synthesisr") == tag_naming)){
      rows <- which(synthesisr::code_lookup[, paste0("ris_", tag_naming)])
      code_lookup_thisfile <- synthesisr::code_lookup[
        rows,
        c("code", "order", "field")
      ]
    }
  }

  # merge data with lookup info, to provide bib-style tags
  x_merge <- merge(x,
    code_lookup_thisfile,
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

  # method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x_merge$text)
  if(any(year_check > 0)){
    check_rows <- which(year_check > 0)
    year_strings <- as.numeric(x_merge$text[check_rows])

    # for entries with a bib entry labelled year, check that there arent multiple years
    if(any(x_merge$field[check_rows] == "year", na.rm = TRUE)){
      # check for repeated year information
      year_freq <- xtabs(~ ref, data = x_merge[which(x_merge$field == "year"), ])
      if(any(year_freq > 1)){
        year_df <- x_merge[which(x_merge$field == "year"), ]
        year_list <- split(nchar(year_df$text), year_df$ris)
        year_4 <- sqrt((4 - unlist(lapply(year_list, mean))) ^ 2)
        # rename bib entries that have >4 characters to 'year_additional'
        incorrect_rows <- which(
          x_merge$ris != names(which.min(year_4)[1]) &
            x_merge$field == "year"
        )
        x_merge$field[incorrect_rows] <- "year_additional"
      }
    }else{
      possible_rows <- which(
        year_strings > 0 &
          year_strings <= as.numeric(format(Sys.Date(), "%Y")) + 1
      )
      tag_frequencies <- as.data.frame(
        xtabs(~ x_merge$ris[check_rows[possible_rows]]),
        stringsAsFactors = FALSE
      )
      colnames(tag_frequencies) <- c("tag", "n")
      # now work out what proportion of each tag contain year data
      # compare against number of references to determine likelihood of being 'the' year tag
      tag_frequencies$prop <- tag_frequencies$n/(max(x_merge$ref)+1) # number of references
      if(any(tag_frequencies$prop > 0.9)){
        year_tag <- tag_frequencies$tag[which.max(tag_frequencies$prop)]
        rows.tr <- which(x_merge$ris == year_tag)
        x_merge$field[rows.tr] <- "year"
        x_merge$row_order[rows.tr] <- 3
      }
    }
  }

  # ensure author data from a single ris tag
  if(any(x_merge$field == "author")){
    lookup.tags <- xtabs( ~ x_merge$ris[which(x_merge$field == "author")])
    if(length(lookup.tags) > 1){
      replace_tags <- names(which(lookup.tags < max(lookup.tags)))
      replace_rows <- which(x_merge$ris %in% replace_tags)
      x_merge$field[replace_rows] <- x_merge$ris[replace_rows]
      if(all(is.na(x_merge$row_order))){
        start_val <- 0
      }else{
        start_val <- max(x_merge$row_order, na.rm = TRUE)
      }
      x_merge$row_order[replace_rows] <- start_val + as.numeric(
        as.factor(x_merge$ris[replace_rows])
      )
    }
  }

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("field", "ris", "text", "order")], x_merge$ref)

  # there is an issue with date accessed creating non-existing records
  # removing datasets with 1 row fixes this
  if(any(unlist(lapply(x_split, nrow))==1)){
    x_split <- x_split[  -which(unlist(lapply(x_split, nrow))==1)]
  }

  # convert to list format
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field)
    # YEAR
    if(any(names(result) == "year")){
      if(any(nchar(result$year) >= 4)){
        year_check <- regexpr("\\d{4}", result$year)
        if(any(year_check > 0)){
          result$year <- substr(
            x = result$year[which(year_check>0)],
            start = year_check[1],
            stop = year_check[1]+3
          )
        }else{
          result$year <- ""
        }
      }else{
        result$year <- ""
      }
    }
    # TITLE
    if(any(names(result) == "title")){
      if(length(result$title) > 1){
        if(result$title[1] == result$title[2]){
          result$title <- result$title[1]
        }else{
          result$title <- paste(result$title, collapse = " ")
        }
      }
      result$title <- gsub("\\s+", " ", result$title) # remove multiple spaces
      result$title <- sub("\\.$", "", result$title) # remove final full stops
    }
    # JOURNAL
    if(any(names(result) == "journal")){
      unique_journals <- unique(result$journal)
      if(length(unique_journals)>1){
        unique_journals <- unique_journals[order(
          nchar(unique_journals),
          decreasing = FALSE
        )]
        result$journal <- unique_journals[1]
        result$journal_secondary <- paste(
          unique_journals[c(2:length(unique_journals))],
          collapse = "; "
        )
      }else{
        result$journal <- unique_journals
      }
      result$journal <-gsub("  ", " ", result$journal)
      result$journal <-sub("\\.$", "", result$journal)
    }
    # ABSTRACT
    if(length(result$abstract > 1)){
      result$abstract <- paste(result$abstract, collapse = " ")
      result$abstract <- gsub("\\s+", " ", result$abstract) # remove multiple spaces
    }
    # PAGE NUMBER
    if(any(names(result) == "pages")){
      if(length(result$pages) > 1){
        result$pages <- paste(sort(result$pages), collapse = "-")
      }
    }

    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    return(result[result_order])
  })

  # names(x_final) <- seq_along(x_final)
  class(x_final) <- "bibliography"
  return(x_final)
}


#' @rdname parse_
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @importFrom unglue unglue_data
#' @export
parse_bibtex <- function(x){
  # use `unglue` to parse text
  raw_df <- unglue_data(x,
              patterns = c("[variable]={[value]},",
                           "@[variable]{[value],"),
              open = "[",
              close = "]")

  # remove missing values
  raw_df <- raw_df[!(is.na(raw_df$variable) | is.na(raw_df$value)), ]

  # create a vector assigning rows to articles
  article_vec <- as.integer(raw_df$variable == "ARTICLE")
  article_vec[is.na(article_vec)] <- 0
  raw_df$article <- cumsum(article_vec)

  # split by article and transpose
  result <- lapply(
    split(raw_df[, 1:2], raw_df$article),
    function(a){
      result <- as.data.frame(t(a$value))
      colnames(result) <- a$variable
      return(result)
    }) |>
    bind_rows() |>
    tibble()

  # split authors
  if(any(names(result) == "author")){
    if(any(grepl("and", result$author))){
      result$author <- strsplit(result$author, "\\s*and\\s*")
    }
  }

  # join duplicated columns
  # note: needs to be done simultaneously with calling `tibble()`

  return(result)
}

#' @rdname parse_
#' @export
parse_csv <- function(x){
  read.table(
    text = x,
    header = TRUE,
    sep = ",",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    stringsAsFactors = FALSE,
    row.names = NULL) |>
  match_columns() |>
  tibble()
}

#' @rdname parse_
#' @export
parse_tsv <- function(x){
  read.table(
    text = x,
    header = TRUE,
    sep = "\t",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    stringsAsFactors = FALSE,
    row.names = NULL) |>
  match_columns() |>
  tibble()
}
