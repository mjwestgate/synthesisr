#' Add line breaks to one or more strings
#' @description This function takes a vector of strings and adds line breaks every n characters. Primarily built to be called internally by format_citation, this function has been made available as it can be useful in other contexts.
#' @param x Either a string or a vector; if the vector is not of class character if will be coerced to one using as.character.
#' @param n Numeric: The number of characters that should separate consecutive line breaks.
#' @param html logical: Should the line breaks be specified in html?
#' @details Line breaks are only added between words, so the value of n is acutally a threshold value rather than being matched exactly.
#' @return Returns the input vector unaltered except for the addition of line breaks.
#' @examples add_line_breaks(c("On the Origin of Species"), n = 10)
add_line_breaks <- function(x, n = 50, html = FALSE){
  if(html){break_string <- "<br>"}else{break_string <- "\n"}
	split_text <- strsplit(as.character(x), " ")
  out_list <- lapply(split_text, function(a){
    if(length(a) == 0){
      return("")
    }else{
      result <- data.frame(
        text = a,
        nchars = nchar(a, allowNA = TRUE, keepNA = TRUE),
        stringsAsFactors = FALSE
      )
      if(any(is.na(result$nchars))){
        result$nchars[which(is.na(result$nchars))] <- 2
      }
    	result$sum <- cumsum(result$nchars)



    	result$group <- cut(result$sum,
    		breaks = seq(0, max(result$sum)+n-1, n),
    		labels = FALSE)



    	result_list <- split(result$text, result$group)

    	while(any(lapply(result_list, function(x){
    	  sum(nchar(x))
    	})>n)){
    	  error_start <- min(which(lapply(result_list, function(x){
    	    sum(nchar(x))
    	  })>n))
    	  error_range <- min(which(result$group==error_start)):nrow(result)

    	  result$sum[error_range] <- cumsum(result$nchars[error_range])

    	  result$group[error_range] <- cut(result$sum[error_range],
    	                      breaks = seq(0, max(result$sum[error_range])+n-1, n),
    	                      labels = FALSE)+result$group[error_range[1]-1]

    	  result_list <- split(result$text, result$group)

    	}

    	result <- paste(
        unlist(
          lapply(result_list, function(a){paste(a, collapse = " ")})
        ),
        collapse = break_string)
      return(result)
    }
  })
  return(unlist(out_list))
}
