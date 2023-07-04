#' Add line breaks to one or more strings
#'
#' This function takes a vector of strings and adds line breaks
#' every n characters. Primarily built to be called internally by
#' `format_citation()`, this function has been made available as it can be
#' useful in other contexts.
#' @param x Either a string or a vector; if the vector is not of class character
#' if will be coerced to one using `as.character()`.
#' @param n Numeric: The desired number of characters that should separate
#' consecutive line breaks.
#' @param html Logical: Should the line breaks be specified in html?
#' @details Line breaks are only added between words, so the value of n is
#' actually a threshold value rather than being matched exactly.
#' @return Returns the input vector unaltered except for the addition of line
#' breaks.
#' @importFrom rlang abort
#' @examples add_line_breaks(c("On the Origin of Species"), n = 10)
#' @export
add_line_breaks <- function(x,
                            n = 50,
                            html = FALSE
                            ){
  if(html){
    break_string <- "<br>"
  }else{
    break_string <- "\n"
  }
  split_text <- strsplit(as.character(x), " ")
  out_list <- lapply(split_text, function(a){
    if(length(a) == 0){
      return("")
    }else{
      result <- data.frame(
        text = a,
        nchars = nchar(a, allowNA = TRUE, keepNA = TRUE) + 1,
        stringsAsFactors = FALSE
      )
      if(any(is.na(result$nchars))){
        result$nchars[which(is.na(result$nchars))] <- 2
      }

      result$group <- cumulative_assign(result$nchars, n)
      result_list <- lapply(split(result$text, result$group),
                            function(a){paste(a, collapse = " ")})
      result <- paste(unlist(result_list), collapse = break_string)
      return(result)
    }
  })
  return(unlist(out_list))
}

#' Internal function to assign words to groups
#'
#' Functions by taking vector of string lengths, and iteratively assigning to
#' groups within a while loop
#' @param x is nchar() of a character vector + 1
#' @param n is the maximum line length allowed
#' @noRd
#' @keywords Internal
cumulative_assign <- function(x, n){
  result_vec <- vector(mode = "integer", length = length(x))
  window_size <- round(n / mean(x) * 2, 0) # this may be too large
  group_value <- 1
  while(any(result_vec < 1)){
    available_rows <- which(result_vec < 1)
    window_tr <- min(c(window_size, length(available_rows)))
    vec_tr <- x[available_rows[seq_len(window_tr)]]
    keep_rows <- which(cumsum(vec_tr) < n)
    result_vec[available_rows[keep_rows]] <- group_value
    group_value <- group_value + 1
  }
  result_vec
}
