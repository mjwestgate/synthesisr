#' @rdname parse_
#' @param tag_naming What format are ris tags in? Defaults to `"best_guess"` See
#' `read_refs()` for a list of accepted arguments.
#' @export
parse_ris <- function(x, tag_naming = "best_guess"){

  # clean up input file
  x <- prep_ris(x,
                detect_delimiter(x),
                type = "generic")

  # merge data with lookup info, to provide bib-style tags
  tag_lookup_thisfile <- get_tag_lookup(x, tag_naming)
  x_merge <- merge(x,
                   tag_lookup_thisfile,
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

  # tidy up specific columns
  x_merge <- x_merge |>
    clean_ris_years() |>
    clean_ris_authors()

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("field", "ris", "text", "order")], x_merge$ref)

  # there is an issue with date accessed creating non-existing records
  # removing datasets with 1 row fixes this
  if(any(unlist(lapply(x_split, nrow))==1)){
    x_split <- x_split[  -which(unlist(lapply(x_split, nrow))==1)]
  }

  # convert to list format
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field) |>
      parse_ris_year() |>
      parse_ris_title() |>
      parse_ris_journal() |>
      parse_ris_abstract() |>
      parse_ris_page_numbers()
    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    return(result[result_order])
  })
  class(x_final) <- "bibliography"
  return(x_final)
}

#' Internal function to clean year data (above)
#' @noRd
#' @keywords Internal
clean_ris_years <- function(x){
  # method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x$text)
  if(any(year_check > 0)){
    check_rows <- which(year_check > 0)
    year_strings <- as.numeric(x$text[check_rows])

    # for entries with a bib entry labelled year, check that there aren't multiple years
    if(any(x$field[check_rows] == "year", na.rm = TRUE)){
      # check for repeated year information
      year_freq <- xtabs(~ ref, data = x[which(x$field == "year"), ])
      if(any(year_freq > 1)){
        year_df <- x[which(x$field == "year"), ]
        year_list <- split(nchar(year_df$text), year_df$ris)
        year_4 <- sqrt((4 - unlist(lapply(year_list, mean))) ^ 2)
        # rename bib entries that have >4 characters to 'year_additional'
        incorrect_rows <- which(
          x$ris != names(which.min(year_4)[1]) &
            x$field == "year"
        )
        x$field[incorrect_rows] <- "year_additional"
      }
    }else{
      possible_rows <- which(
        year_strings > 0 &
          year_strings <= as.numeric(format(Sys.Date(), "%Y")) + 1
      )
      tag_frequencies <- as.data.frame(
        xtabs(~ x$ris[check_rows[possible_rows]]),
        stringsAsFactors = FALSE
      )
      colnames(tag_frequencies) <- c("tag", "n")
      # now work out what proportion of each tag contain year data
      # compare against number of references to determine likelihood of being 'the' year tag
      tag_frequencies$prop <- tag_frequencies$n/(max(x$ref)+1) # number of references
      if(any(tag_frequencies$prop > 0.9)){
        year_tag <- tag_frequencies$tag[which.max(tag_frequencies$prop)]
        rows.tr <- which(x$ris == year_tag)
        x$field[rows.tr] <- "year"
        x$row_order[rows.tr] <- 3
      }
    }
  }
  x
}

#' Internal function to clean author data (above)
#' @noRd
#' @keywords Internal
clean_ris_authors <- function(x){
  # ensure author data from a single ris tag
  if(any(x$field == "author")){
    lookup.tags <- xtabs( ~ x$ris[which(x$field == "author")])
    if(length(lookup.tags) > 1){
      replace_tags <- names(which(lookup.tags < max(lookup.tags)))
      replace_rows <- which(x$ris %in% replace_tags)
      x$field[replace_rows] <- x$ris[replace_rows]
      if(all(is.na(x$row_order))){
        start_val <- 0
      }else{
        start_val <- max(x$row_order, na.rm = TRUE)
      }
      x$row_order[replace_rows] <- start_val + as.numeric(
        as.factor(x$ris[replace_rows])
      )
    }
  }
  x
}

#' Internal function to build a tag lookup table
#' @noRd
#' @keywords Internal
get_tag_lookup <- function(x, tag_naming){
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
  code_lookup_thisfile
}

#' Internal function to handle abstracts
#' @noRd
#' @keywords Internal
parse_ris_abstract <- function(result){
  if(length(result$abstract > 1)){
    result$abstract <- paste(result$abstract, collapse = " ")
    result$abstract <- gsub("\\s+", " ", result$abstract) # remove multiple spaces
  }
  result
}

#' Internal function to handle years
#' @noRd
#' @keywords Internal
parse_ris_year <- function(result){
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
  result
}

#' Internal function to handle titles
#' @noRd
#' @keywords Internal
parse_ris_title <- function(result){
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
  result
}

#' Internal function to handle journals
#' @noRd
#' @keywords Internal
parse_ris_journal <- function(result){
  if(any(names(result) == "journal")){
    unique_journals <- unique(result$journal)
    if(length(unique_journals) > 1){
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
  result
}

#' Internal function to handle page numbers
#' @noRd
#' @keywords Internal
parse_ris_page_numbers <- function(result){
  if(any(names(result) == "pages")){
    if(length(result$pages) > 1){
      result$pages <- paste(sort(result$pages), collapse = "-")
    }
  }
  result
}
