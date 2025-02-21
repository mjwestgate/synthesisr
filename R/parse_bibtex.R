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
