# Duplicate of functions from the Python library fuzzywuzzy
#github .com/seatgeek/fuzzywuzzy
# these functions coded by Martin Westgate on 4th June 2018 based on description given here:
#chairnerd.seatgeek .com/fuzzywuzzy-fuzzy-string-matching-in-python/

#' Detect duplicates with fuzzy matching
#' @description This is a wrapper function for the different fuzzy matching methods.
#' @param a A character vector of items to match to b.
#' @param b A character vector of items to match to a.
#' @param method The method to use for fuzzy matching.
#' @note fuzz_m_ratio is a measure of the number of letters that match between two strings. It is calculated as one minus two times the number of matched characters, divided by the number of characters in both strings.
#' @note fuzz_partial_ratio calculates the extent to which one string is a subset of the other. If one string is a perfect subset, then this will be zero.
#' @note fuzz_token_sort_ratio sorts the words in both strings into alphabetical order, and checks their similarity using fuzz_m_ratio.
#' @note fuzz_token_set_ratio is similar to fuzz_token_sort_ratio, but compares both sorted strings to each other, and to a third group made of words common to both strings. It then returns the maximum value of fuzz_m_ratio from these comparisons.
#' @note fuzzdist is a wrapper function, for compatability with stringdist.
#' @return Returns a score of same length as b, giving the proportional dissimilarity between a and b.
#' @example inst/examples/fuzzdist.R
fuzzdist <- function(a, b,
  method = c("fuzz_m_ratio", "fuzz_partial_ratio",
             "fuzz_token_sort_ratio", "fuzz_token_set_ratio")
){
  method <- match.arg(method)
  do.call(
    method,
    list(a, b)
  )
}

#' @describeIn fuzzdist Method simple ratio
fuzz_m_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      z_list <- lapply(strsplit(z, ""),
        function(x, minval){x[1:minval]},
        minval = min(nchar(z))
      )
      z_match <- apply(
        do.call(cbind, z_list),
        1,
        function(x){x[1] == x[2]}
      )
      return(
        1 - (2 * length(which(z_match)) / sum(nchar(z)))
      )
    }
  },
  a = a)
  return(as.numeric(out))
}


#' @describeIn fuzzdist Method partial ratio
fuzz_partial_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      zn <- nchar(z)
      n_reps <- (max(zn) - min(zn))
      z_list <- lapply(
        c(0: n_reps),
        function(x, lookup, keep){lookup[(keep + x)]},
        lookup = strsplit(z[which.max(zn)], "")[[1]],
        keep = seq_len(min(zn))
      )
      z_ratio <- lapply(z_list, function(x, comparison){
      	match_value <- apply(
          cbind(x, comparison),
          1,
          function(y){y[1] == y[2]}
        )
      	length(which(match_value))/length(x)
      },
      comparison = strsplit(z[which.min(zn)], "")[[1]]
      )
      return(1 - max(as.numeric(z_ratio)))
    }
  },
  a = a)
  return(as.numeric(out))
}


#' @describeIn fuzzdist Method token sort ratio
fuzz_token_sort_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      z_list <- lapply(
        strsplit(z, " "),
        function(x){paste(sort(x), collapse = " ")}
      )
      return(
        fuzz_m_ratio(z_list[[1]], z_list[[2]])
      )
    }
  },
  a = a)
  return(as.numeric(out))
}

#' @describeIn fuzzdist Method token set ratio
fuzz_token_set_ratio <- function(a, b){
  out <- lapply(b, function(b, a){
    z <- c(a, b)
    if(any(is.na(z))){
      return(NA)
    }else{
      z_split <- strsplit(z, " ")
      in_check <- z_split[[1]] %in% z_split[[2]]
      intersection <- sort(z_split[[1]][which(in_check)])
      string_list <- list(
        t0 = intersection,
        t1 = c(intersection,
          sort(z_split[[1]][which(!in_check)])
        ),
        t2 = c(intersection,
          sort(z_split[[2]][which(!(z_split[[2]] %in% intersection))])
        )
      )
      string_list <- lapply(string_list, function(x){
        if(length(x) < 1){
          return("")
    	  }else{
          return(paste(x, collapse = " "))
    	  }
      })
      result <- c(
        fuzz_m_ratio(string_list$t0, string_list$t1),
        fuzz_m_ratio(string_list$t0, string_list$t2),
        fuzz_m_ratio(string_list$t1, string_list$t2)
        )
      return(max(result))
    }
  },
  a = a)
  return(as.numeric(out))
}
