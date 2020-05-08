#' Detect file formatting information
#'
#' @description Bibliographic data can be stored in a number of different file types, meaning that detecting consistent attributes of those files is necessary if they are to be parsed accurately. These functions attempt to identify some of those key file attributes. Specifically, \code{detect_parser} determines which \code{\link{parse_}} function to use; \code{detect_delimiter} and \code{detect_lookup} identify different attributes of RIS files; and \code{detect_year} attempts to fill gaps in publication years from other information stored in a \code{data.frame}.
#' @param x A character vector containing bibliographic data
#' @param tags A character vector containing RIS tags.
#' @param df a data.frame containing bibliographic data
#' @return \code{detect_parser} and \code{detect_delimiter} return a length-1 character; \code{detect_year} returns a character vector listing estimated publication years; and \code{detect_lookup} returns a \code{data.frame}.
#' @example inst/examples/detect_.R
#' @name detect_
NULL
