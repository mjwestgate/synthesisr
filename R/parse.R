#' Parse bibliographic text in a variety of formats
#'
#' @description Text in standard formats - such as imported via \code{\link{readLines}} - can be parsed using a variety of standard formats. Use \code{\link{detect_parser}} to determine which is the most appropriate parser for your situation.
#' @param x A character vector containing bibliographic information in ris format.
#' @return Returns an object of class bibliography.
#' @example inst/examples/parse_ris.R
#' @name parse
NULL