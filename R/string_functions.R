# string functions

#' @rdname string_
string_osa <- function(a, b){stringdist(a, b, method = "osa")}

#' @rdname string_
string_lv <- function(a, b){stringdist(a, b, method = "lv")}

#' @rdname string_
string_dl <- function(a, b){stringdist(a, b, method = "dl")}

#' @rdname string_
string_hamming <- function(a, b){stringdist(a, b, method = "hamming")}

#' @rdname string_
string_lcs <- function(a, b){stringdist(a, b, method = "lcs")}

#' @rdname string_
string_qgram <- function(a, b){stringdist(a, b, method = "qgram")}

#' @rdname string_
string_cosine <- function(a, b){stringdist(a, b, method = "cosine")}

#' @rdname string_
string_jaccard <- function(a, b){stringdist(a, b, method = "jaccard")}

#' @rdname string_
string_jw <- function(a, b){stringdist(a, b, method = "jw")}

#' @rdname string_
string_soundex <- function(a, b){stringdist(a, b, method = "soundex")}
