#' Calculate similarity between two strings
#' @description These functions each access a specific \code{"methods"} argument provided by \code{stringdist}, and are provided for convenient calling by \code{\link{find_duplicates}}. They do not include any new functionality beyond that given by \code{stringdist}, which you should use for your own analyses.
#' @param a A character vector of items to match to b.
#' @param b A character vector of items to match to a.
#' @return Returns a score of same length as b, giving the dissimilarity between a and b.
#' @name string_
NULL
