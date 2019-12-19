#' Bibliographic code lookup for search results assembly
#'
#' A data frame containing that can be used to look up
#' common codes for different bibliographic fields
#' across databases and merge them to a common format
#'
#' @format A data frame with 207 obs of 3 variables
#'
#' \describe{
#'  \item{code}{code used in search results}
#'  \item{description}{description of field from original source}
#'  \item{field}{bibliographic field that codes correspond to}
#'  \item{order}{the order in which to rank fields in assembled results}}
#'
"code_lookup"

#' Languages codes synthesisr can recognize
#'
#' A dataset of the languages that searches can be recognized
#' by synthesisr along with their short form, character encoding,
#' and whether a scientific journal indexed in ulrich uses them.
#'
#' @format A database with 53 rows of 4 variables:
#' \describe{
#'   \item{Short}{the short form language code}
#'   \item{Language}{the name of the language}
#'   \item{Encoding}{which character encoding to use for a language}
#'   \item{Used}{whether or not the language is used by a scientific journal}
#' }
"possible_langs"
