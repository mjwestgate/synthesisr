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
