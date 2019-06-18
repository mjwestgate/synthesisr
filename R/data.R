#' Information about databases recognized by synthesisr
#'
#' A data frame containing databases, database signatures
#' used to recognize databases, and which function should
#' be called to import results from that database.
#'
#' @format A data frame with 18 obs of 3 variables
#'
#' \describe{
#'  \item{database}{the name of the database}
#'  \item{signature}{a unique signature that appears in search results from this database}
#'  \item{import_function}{which import function to use if this database is detected}}
#'
"databases"

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
