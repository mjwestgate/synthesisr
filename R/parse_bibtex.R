#' @rdname parse_
#' @export
parse_bibtex <- function(x){

  # on rare occasions, for example when text is added in the console,
  # text arrives as a length-1 vector with embedded `\n`s for line breaks.
  # BUT parsing `\n` is risky otherwise as it may be present in e.g.
  # abstracts. So restrict this parsing for now.
  if(length(x) == 1L){
    x_split <- strsplit(x, "\\n") |>
      unlist() # unlist re-groups content that is split unnecessarily back into a vector
  }else{
    x_split <- x
  }

  # determine which values start a row
  open_tag <- stringr::str_detect(x_split, "@[[:alnum:]]+\\{") |>
    as.integer()
  open_tag[is.na(open_tag)] <- 0

  # use `unglue` to parse text
  # 1. Matching reference to the type of reference (e.g. article)
  # 2. Matches each of the variables to their corresponding values
  #   - Removes any white space out the front of the variable name
  #   - Allows zero or more spaces around the equals sign
  #   - Comma at the end is optional
  raw_df <- unglue::unglue_data(x_split,
                                patterns = c("@[variable]{[value],",
                                             "[=\\s*][variable][=\\s*=\\s*]{[value]}[=,?]"),
                                open = "[",
                                close = "]")  |>
    tibble::as_tibble()

  if(nrow(raw_df) > 0){
    raw_df <- raw_df |>
      dplyr::mutate(entry = cumsum(open_tag)) |> # create a vector assigning rows to articles
      dplyr::filter(!is.na(.data$variable)) # remove missing values

    # split by entry and transpose
    split_df <- split(raw_df[, 1:2], raw_df$entry)
    result <- lapply(split_df,
                     function(a){
                       b <- split(a$value, a$variable)}) |>
      purrr::list_transpose() |>
      as_tibble()

    # split authors
    if(any(names(result) == "author")){
      if(any(stringr::str_detect(result$author, "and"))){
        result$author <- strsplit(result$author, "\\s*and\\s*")
      }
    }

    return(result)
  }else{
    tibble()
  }
}
