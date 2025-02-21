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

#' Internal function used by parse_csv and parse_tsv:
#' Matches imported data to reference codes
#'
#' @description Takes an imported data.frame and rearranges it to match lookup
#' codes.
#' @param df A data.frame that contains bibliographic information.
#' @return Returns a data.frame rearranged and coded to match standard
#' bibliographic fields, with unrecognized fields appended.
#' @noRd
#' @keywords Internal
#' @example inst/examples/match_columns.R
match_columns <- function(df){
  # figure out which columns match known tags
  hits <- as.numeric(match(synthesisr::code_lookup$code, colnames(df)))
  newcolnames <- synthesisr::code_lookup$field[
    match(colnames(df),
          synthesisr::code_lookup$code)
  ]
  colnames(df)[!is.na(newcolnames)] <- newcolnames[!is.na(newcolnames)]

  # rearrange data in standard(ish) order
  if(any(is.na(hits))){
    hits <- hits[!is.na(hits)]
  }

  # retain columns even if they did not match lookup
  retain <- append(hits, seq(1, length(df), 1)[!(seq(1, length(df), 1) %in% hits)])

  return(df[,retain])
}
