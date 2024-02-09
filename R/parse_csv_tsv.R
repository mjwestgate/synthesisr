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
