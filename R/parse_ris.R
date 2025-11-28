#' @rdname parse_
#' @param tag_naming What format are ris tags in? Defaults to `"best_guess"` See
#' `read_refs()` for a list of accepted arguments.
#' @export
parse_ris <- function(x, tag_naming = "best_guess"){

  # clean up input file
  x <- prep_ris(x,
                detect_delimiter(x),
                type = "generic")

  if(tag_naming == "none"){
    x_merge <- x |>
      dplyr::mutate("field" = .data[["ris"]],
                    "order" = dplyr::row_number())
  }else{
    # handle lookup for tag renaming
    # find a lookup table
    tag_lookup_thisfile <- get_tag_lookup(x, tag_naming)

    # join to source data
    x_merge <- dplyr::left_join(x,
                                tag_lookup_thisfile,
                                by = c("ris" = "code")) |>
      dplyr::arrange(.data[["row_order"]])

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
  }

  # convert into a list, where each reference is a separate entry
  x_split <- x_merge |>
    dplyr::select("field", "ris", "text", "order") |>
    split(dplyr::pull(x_merge, "ref"))

  # # there is an issue with date accessed creating non-existing records
  # # removing datasets with 1 row fixes this
  # if(any(unlist(lapply(x_split, nrow)) == 1)){
  #   x_split <- x_split[  -which(unlist(lapply(x_split, nrow)) == 1)]
  # }

  # convert to list format
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field) |>
      parse_ris_author() |>
      parse_ris_year() |>
      parse_ris_title() |>
      parse_ris_journal() |>
      parse_ris_abstract() |>
      parse_ris_page_numbers()
    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    result[result_order]
  })
  structure(x_final, class = "bibliography")
}

#' Internal function to clean year data (above)
#' @param x A tibble
#' @noRd
#' @keywords Internal
clean_ris_years <- function(x){
  # method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x$text)
  year_check <- stringr::str_detect(x$text, "^\\d{4}$")

  if(any(year_check)){
    check_rows <- which(year_check)
    year_strings <- as.numeric(x$text[check_rows])

    # for entries with a bib entry labelled year, check that there aren't multiple years
    field_check <- x$field[check_rows] == "year"
    if(any(field_check, na.rm = TRUE)){

      # check for repeated year information
      year_freq <- x |>
        dplyr::filter(.data$field == "year") |>
        dplyr::group_by(.data$ref) |>
        dplyr::count() |>
        dplyr::pull("n")

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
          year_strings <= as.numeric(format(Sys.Date(), "%Y")) + 1)

      # now work out what proportion of each tag contain year data
      # compare against number of references to determine likelihood of being 'the' year tag
      tag_frequencies <- x |>
        dplyr::slice(check_rows[possible_rows]) |>
        dplyr::group_by(.data$ris) |>
        dplyr::count() |>
        dplyr::mutate(prop = .data$n / (max(x$ref) + 1)) # number of references

      if(any(tag_frequencies$prop > 0.9)){
        year_tag <- tag_frequencies$ris[which.max(tag_frequencies$prop)]
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
#' @param x a tibble
#' @param tag_naming a string
#' @noRd
#' @keywords Internal
get_tag_lookup <- function(x,
                           tag_naming = "best_guess"){
  if(missing(x)){
    abort("`x` is missing, with no default")
  }
  if(!inherits(x, "data.frame")){
    abort("Argument `x` must inherit from class `data.frame`")
  }
  ris_vec <- dplyr::pull(x, "ris")
  if(is.null(ris_vec)){
    abort("`x` must contain a column named `ris`")
  }

  # create the appropriate lookup file for the specified tag
  if(inherits(tag_naming, "data.frame")){
    if(!any(colnames(tag_naming) == "order")){
      tag_naming$order <- seq_len(nrow(tag_naming))
    }
    tag_naming
  }else{
    if(tag_naming == "none"){
      ris_vals <- unique(x$ris)
      tibble(code = ris_vals,
             field = ris_vals,
             order = seq_along(ris_vals))
    }else if(tag_naming == "best_guess"){
      detect_lookup(tags = unique(x$ris))
    }else if(any(c("wos", "scopus", "ovid", "asp", "synthesisr") == tag_naming)){
      tag_column <- glue::glue("ris_{tag_naming}")
      synthesisr::code_lookup |>
        dplyr::filter(.data[[tag_column]] == TRUE) |>
        dplyr::select("code", "order", "field")
    }else{
      abort("argument `tag_naming` not recognized")
    }
  }
}

#' Internal function to handle authors
#' @noRd
#' @keywords Internal
parse_ris_author <- function(result){
  if(any(names(result) == "author")){
    result$author <- as.list(result$author)
  }
  result
}

#' Internal function to handle abstracts
#' @noRd
#' @keywords Internal
parse_ris_abstract <- function(result){
  if(length(result$abstract > 1)){
    result$abstract <- result$abstract |>
      glue::glue_collapse(sep = " ") |> # collapse to a single string
      stringr::str_replace_all("\\s+", " ") |>  # remove multiple spaces
      as.character()
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
          x = result$year[which(year_check > 0)],
          start = year_check[1],
          stop = year_check[1] + 3)
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
        # Note: concatenation assumes line breaks are parsed as separate
        # entries. For other use cases this approach doesn't make sense
        result$title <- glue::glue_collapse(result$title, sep = " ") |>
          as.character()
      }
    }
    result$title <- stringr::str_replace_all(result$title, "\\s+", " ")
    result$title <- stringr::str_replace(result$title, "\\.$", "")
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
      result$journal_secondary <- glue::glue_collapse(
        unique_journals[c(2:length(unique_journals))],
        sep = "; ") |>
        as.character()
    }else{
      result$journal <- unique_journals
    }
    result$journal <- stringr::str_replace_all(result$journal, "\\s+", " ")
    result$journal <- stringr::str_replace(result$journal, "\\.$", "")
  }
  result
}

#' Internal function to handle page numbers
#' @param result a named list
#' @noRd
#' @keywords Internal
parse_ris_page_numbers <- function(result){
  if(any(names(result) == "pages")){
    if(length(result$pages) > 1){
      result$pages <- sort(result$pages) |>
        glue::glue_collapse(sep = "-") |>
        as.character()
    }
  }
  result
}
