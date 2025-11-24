#' @rdname parse_
#' @export
parse_bibtex <- function(x){
  # use `unglue` to parse text
  # 1. Matching reference to the type of reference (e.g. article)
  # 2. Matches each of the variables to their corresponding values
  #   - Removes any white space out the front of the variable name
  #   - Allows zero or more spaces around the equals sign
  #   - Comma at the end is optional
  raw_df <- unglue::unglue_data(x,
                                patterns = c("@[variable]{[value],",
                                             "[=\\s*][variable][=\\s*=\\s*]{[value]}[=,?]"),
                                open = "[",
                                close = "]")

  # remove missing values
  raw_df <- raw_df[!(is.na(raw_df$variable) | is.na(raw_df$value)), ]

  # create a vector assigning rows to articles
  article_vec <- as.integer(raw_df$variable == "ARTICLE")
  article_vec[is.na(article_vec)] <- 0
  raw_df$article <- cumsum(article_vec)

  # split by article and transpose
  split_df <- split(raw_df[, 1:2], raw_df$article)
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
}
