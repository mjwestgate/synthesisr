#' Bibliographic code lookup for search results assembly
#'
#' A data frame that can be used to look up common
#' codes for different bibliographic fields across
#' databases and merge them to a common format.
#'
#' @format A data frame with 207 obs of 4 variables
#'
#' \describe{
#'  \item{code}{code used in search results}
#'  \item{order}{the order in which to rank fields in assembled results}
#'  \item{category_description}{type of bibliographic data}
#'  \item{entry_description}{description of field}
#'  \item{field}{bibliographic field that codes correspond to}
#'  \item{ris_generic}{logical: if the code is used in generic ris files}
#'  \item{ris_wos}{logical: if the code is used in Web of Science ris files}
#'  \item{ris_pubmed}{logical: if the code is used in PubMed ris files}}
#'
"code_lookup"

#' Languages codes synthesisr can recognize
#'
#' A dataset of the languages that can be recognized by
#' synthesisr along with their short form, character encoding,
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
