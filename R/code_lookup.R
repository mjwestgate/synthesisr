#' Bibliographic code lookup for search results assembly
#'
#' A data frame that can be used to look up common
#' codes for different bibliographic fields across
#' databases and merge them to a common format.
#'
#' @format A data frame with 226 obs of 12 variables
#'
#' \describe{
#'  \item{code}{code used in search results}
#'  \item{order}{the order in which to rank fields in assembled results}
#'  \item{category_description}{type of bibliographic data}
#'  \item{entry_description}{description of field}
#'  \item{field}{bibliographic field that codes correspond to}
#'  \item{ris_generic}{logical: If the code is used in generic ris files}
#'  \item{ris_wos}{logical: If the code is used in Web of Science ris files}
#'  \item{ris_pubmed}{logical: If the code is used in PubMed ris files}
#'  \item{ris_scopus}{logical: If the code is used in Scopus ris files}
#'  \item{ris_asp}{logical: If the code is used in Academic Search Premier ris files}
#'  \item{ris_ovid}{logical: If the code is used in Ovid ris files}
#'  \item{ris_synthesisr}{logical: If the code used in synthesisr imports & exports}}
#'
"code_lookup"
