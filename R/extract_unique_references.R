#' Remove duplicates from a bibliiographic data set
#' @description Given a list of duplicate entries and a data set, extracts only unique references
#' @param x a data.frame in the format returned by make_reference_dataframe()
#' @param matches a character vector showing which values are duplicates
#' @return a data.frame of unique references
#' @example inst/examples/deduplicate.R
deduplicate <- function(
	x, # data.frame returned by make_reference_dataframe
  matches # vector showing which values are duplicates
	){
  if(missing(matches)){
    stop("please specify a vector containing identified matches
    (e.g. as returned by find_duplicates)")
  }
  if(length(matches) == 1){
    matches <- x[, matches]
  }
	x_split <- split(x, matches)
	x_split <- lapply(x_split, function(a){
    if(nrow(a) == 1){
      result <- a[1, ]
      result$n_duplicates <- 1
    }else{
		  row <- which.max(
        apply(
          apply(a, 1, nchar),
          2,
          function(b){sum(b, na.rm = TRUE)}
        )
      )
      result <- a[row, ]
		  result$n_duplicates <- nrow(a)
    }
		return(result)
  })
	output <- as.data.frame(
    do.call(rbind, x_split),
    stringsAsFactors = FALSE
  )
	return(output)
	}
