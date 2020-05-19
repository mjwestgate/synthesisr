# Functions from the 'fuzzywuzzy' Python library
#github .com/seatgeek/fuzzywuzzy
# these functions coded by Martin Westgate on 4th June 2018 based on description given here:
#chairnerd.seatgeek.com/fuzzywuzzy-fuzzy-string-matching-in-python/

#' Calculate similarity between two strings
#' @description These functions duplicate the approach of the 'fuzzywuzzy' Python library for calculating string similarity.
#' @param a A character vector of items to match to b.
#' @param b A character vector of items to match to a.
#' @param method The method to use for fuzzy matching.
#' @note \code{fuzz_m_ratio} is a measure of the number of letters that match between two strings. It is calculated as one minus two times the number of matched characters, divided by the number of characters in both strings.
#' @note \code{fuzz_partial_ratio} calculates the extent to which one string is a subset of the other. If one string is a perfect subset, then this will be zero.
#' @note \code{fuzz_token_sort_ratio} sorts the words in both strings into alphabetical order, and checks their similarity using fuzz_m_ratio.
#' @note \code{fuzz_token_set_ratio} is similar to fuzz_token_sort_ratio, but compares both sorted strings to each other, and to a third group made of words common to both strings. It then returns the maximum value of fuzz_m_ratio from these comparisons.
#' @note \code{fuzzdist} is a wrapper function, for compatability with \code{stringdist}.
#' @return Returns a score of same length as b, giving the proportional dissimilarity between a and b.
#' @example inst/examples/fuzzdist.R
#' @name fuzz_
NULL
