#' Calculate similarity between two strings
#' @description These functions each access a specific `"methods"` argument
#' provided by `stringdist`, and are provided for convenient calling by
#' `find_duplicates()`. They do not include any new functionality beyond that
#' given by `stringdist`, which you should use for your own analyses.
#' @param a A character vector of items to match to b.
#' @param b A character vector of items to match to a.
#' @return Returns a score of same length as b, giving the dissimilarity between
#' a and b.
#' @importFrom stringdist stringdist
#' @name string_
#' @export
string_osa <- function(a, b){stringdist(a, b, method = "osa")}

## NOTE: This looks like poor coding practice. Consider deprecating.

#' @rdname string_
#' @export
string_lv <- function(a, b){stringdist(a, b, method = "lv")}

#' @rdname string_
#' @export
string_dl <- function(a, b){stringdist(a, b, method = "dl")}

#' @rdname string_
#' @export
string_hamming <- function(a, b){stringdist(a, b, method = "hamming")}

#' @rdname string_
#' @export
string_lcs <- function(a, b){stringdist(a, b, method = "lcs")}

#' @rdname string_
#' @export
string_qgram <- function(a, b){stringdist(a, b, method = "qgram")}

#' @rdname string_
#' @export
string_cosine <- function(a, b){stringdist(a, b, method = "cosine")}

#' @rdname string_
#' @export
string_jaccard <- function(a, b){stringdist(a, b, method = "jaccard")}

#' @rdname string_
#' @export
string_jw <- function(a, b){stringdist(a, b, method = "jw")}

#' @rdname string_
#' @export
string_soundex <- function(a, b){stringdist(a, b, method = "soundex")}
