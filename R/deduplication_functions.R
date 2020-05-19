#' Detect duplicate values
#'
#' @description Identifies duplicate bibliographic entries using different duplicate detection methods.
#' @param data A character vector containing duplicate bibliographic entries.
#' @param method A string indicating how matching should be calculated. Either \code{"exact"} for exact matching (the default), or the name of a function for calculating string distance.
#' @param group_by An optional vector, data.frame or list containing data to use as 'grouping' variables; that is, categories within which duplicates should be sought. Defaults to NULL, in which case all entries are compared against all others. Ignored if \code{method = "exact"}.
#' @param threshold Numeric: the cutoff threshold for deciding if two strings are duplcates. Sensible values depend on the \code{method} chosen. Defaults to 5 is \code{method = "string_osa"} and must be specified in all other instances except \code{method = "exact"} (where no threshold is required).
#' @param to_lower Logical: Should all entries be converted to lower case before calculating string distance? Defaults to FALSE.
#' @param rm_punctuation Logical: Should punctuation should be removed before calculating string distance? Defaults to FALSE.
#' @return Returns a vector of duplicate matches, with \code{attributes} listing methods used.
#' @seealso \code{\link{string_}} or \code{\link{fuzz_}} for suitable functions to pass to \code{methods}; \code{\link{extract_unique_references}} and \code{\link{deduplicate}} for higher-level functions.
#' @example inst/examples/deduplicate.R
find_duplicates <- function(
  data, # string
  method = "exact",
  group_by, # either a vector or a data.frame or list containing vectors
  threshold,
  to_lower = FALSE,
  rm_punctuation = FALSE
){
  # data
  if(missing(data)){
    stop("'data' is missing: Please provide a data.frame")
  }
  if(inherits(data, "data.frame")){
    stop("'data' must be a character vector, not a data.frame")
  }
  if(class(data) != "character"){
    data <- as.character(data)
  }

  # grouping
  if(missing(group_by)){
    group_variables <- NULL
  }else{
    if(!inherits(group_by, c("data.frame", "character", "list"))){
      stop("object specified by 'group_by' must be of class list, data.frame or character")
    }
    group_variables <- switch(class(group_by),
      "character" = {list(group_by)},
      "data.frame" = {as.list(group_by)},
      "list" = {group_by}
    )
  }

  # threshold
  if(missing(threshold)){
    if(method == "exact"){
      threshold <- NULL
    }else if(method == "string_osa"){
      threshold <- 5
    }else{
      stop("threshold is missing, with no default")
    }
  }

  # transformations
  if(to_lower){data <- tolower(data)}
  if(rm_punctuation){data <- gsub("[[:punct:]]", "", data)}

  # quick option for exact matching based on split()
  if(method == "exact"){
    order_initial <- seq_along(data)
    # ensure NAs are always given a unique value
    data[is.na(data)] <- paste0("MISSING_VALUE_", seq_along(which(is.na(data))))
    # split data by name
    order_list <- split(order_initial, data)
    order_list <- order_list[order(unlist(lapply(order_list, min)))]
    result <- do.call(c, lapply(
      seq_along(order_list),
      function(a, ol){rep(a, length(ol[[a]]))},
      ol = order_list
    ))
    result <- result[order(do.call(c, order_list))]

  }else{ # i.e. if a distance-based method is requested

    # prep vectors to track progress
    checked <- rep(FALSE, length(data))
    group <- rep(FALSE, length(data))

    # run 'while' loop
    progress <- 1
    while(all(checked) == FALSE){
      remaining_rows <- which(checked == FALSE)
      if(length(remaining_rows) == 1){
        group[remaining_rows] <- progress
        checked[remaining_rows] <- TRUE
      }else{
        # locate relevant information
        row_start <- remaining_rows[1]
        # if this entry is empty, then skip (i.e. never match NAs)
        if(is.na(data[row_start])){
          checked[row_start] <- TRUE
          group[row_start] <- progress
        }else{
          # include only those entries in the same grouping categories as the current entry,
          # plus any entries that are missing those values
          if(is.null(group_variables)){
            rows_tr <- remaining_rows
          }else{
            match_list <- lapply(
              group_variables,
              function(a, row){
                if(is.na(a[row])){rep(TRUE, length(a))}else{a == a[row]}
              }, row = row_start
            )
            if(length(group_variables) == 1){
              rows_tr <- which(unlist(match_list))
            }else{
              rows_tr <- which(apply(
                do.call(cbind, match_list),
                1,
                function(a){all(a)}
              ))
            }
          }
          rows_tr <- rows_tr[which(rows_tr != row_start)]

          if(length(rows_tr) > 0){
            match_result <- do.call(
              method,
              list(
                a = data[row_start],
                b = data[rows_tr]
              )
            ) <= threshold
            if(any(match_result, na.rm = TRUE)){
              rows_selected <- rows_tr[which(match_result)]
              checked[c(row_start, rows_selected)] <- TRUE
              group[c(row_start, rows_selected)] <- progress
            }else{
              checked[row_start] <- TRUE
              group[row_start] <- progress
            }
          }else{
            checked[row_start] <- TRUE
            group[row_start] <- progress
          }
        } # end if(is.na(data[row_start, match_variable]))
      } # end if(length(remaining_rows) == 1)
      progress <- progress + 1
    } # end while loop
    result <- group
  }

  # add attributes
  attr(result, "method") <- method
  attr(result, "threshold") <- threshold
  attr(result, "to_lower") <- to_lower
  attr(result, "rm_punctuation") <- rm_punctuation
  return(result)
}

#' Remove duplicates from a bibliographic data set
#'
#' @description Given a list of duplicate entries and a data set, this function extracts only unique references.
#' @param data A \code{data.frame} containing bibliographic information.
#' @param matches A vector showing which entries in \code{data} are duplicates.
#' @param type How should entries be selected to retain? Default is \code{"merge"} which selects the entries with the largest number of characters in each column. Alternatively \code{"select"} which returns the row with the highest total number of characters.
#' @return Returns a \code{data.frame} of unique references.
#' @seealso \code{\link{find_duplicates}}, \code{\link{deduplicate}}
#' @example inst/examples/deduplicate.R
extract_unique_references <- function(
  data, # data.frame
  matches, # vector showing which values are duplicates
  type = "merge"
){
  if(missing(matches)){
    stop("please specify a vector containing identified matches
    (e.g. as returned by find_duplicates)")
  }
  if(length(matches) != nrow(data)){
    stop("'matches' does not have the same length as x")
  }
  if(!type %in% c("merge", "select")){
    stop("'type' must be one of 'select' or 'merge'")
  }

  x_split <- split(data, matches)
  x_split <- lapply(x_split, function(a, type){
    if(nrow(a) == 1){
      result <- a[1, ]
      result$n_duplicates <- 1
    }else{
      if(type == "merge"){
        result_list <- lapply(a, function(b){
          nx <- nchar(as.character(b))
          nx[is.na(nx)] <- 0
          return(b[which.max(nx)])
        })
        result <- as.data.frame(result_list, stringsAsFactors = FALSE)
      }else{
        row <- which.max(
          apply(
            apply(a, 1, nchar),
            2,
            function(b){sum(b, na.rm = TRUE)}
          )
        )
        result <- a[row, ]
      }
      result$n_duplicates <- nrow(a)
    }
    return(result)
  }, type = type
  )
  output <- as.data.frame(
    do.call(rbind, x_split),
    stringsAsFactors = FALSE
  )
  return(output)
}


#' Remove duplicates from a bibliographic data set
#'
#' @description Removes duplicates using sensible defaults
#'
#' @param data A \code{data.frame} containing bibliographic information.
#' @param match_by Name of the column in \code{data} where duplicates should be sought.
#' @param method The duplicate detection function to use; see see \code{link{string_}} or \code{link{fuzz_}} for examples. Passed to \code{find_duplicates}.
#' @param type How should entries be selected? Default is \code{"merge"} which selected the entries with the largest number of characters in each column. Alternatively \code{"select"} returns the row with the highest total number of characters.
#' @param \dots Arguments passed to \code{find_duplicates}.
#' @return A \code{data.frame} containing data identified as unique.
#' @details
#' This is a wrapper function to \code{\link{find_duplicates}} and \code{extract_unique_references}, which tries to choose some sensible defaults. Use with care.
#' @seealso \code{\link{find_duplicates}} and \code{\link{extract_unique_references}} for underlying functions.
#' @example inst/examples/deduplicate.R
deduplicate <- function(
  data,
  match_by,
  method,
  type = "merge",
  ... # args passed to find_duplicates
){
  if(missing(data)){
    stop("'data' is missing, with no default")
  }

  # add defaults
  if(missing(match_by)){
    if(any(colnames(data) == "doi")){
      data_fd <- data[, "doi"]
      if(missing(method)){method <- "exact"}
    }else{
      if(any(colnames(data) == "title")){
        data_fd <- data[, "title"]
        if(missing(method)){method <- "string_osa"}
      }else{
        stop("'match_by' is missing, with no default;
          please specify which column should be searched for duplicates"
        )
      }
    }
  }else{
    if(!any(colnames(data) == match_by)){
      stop(paste0(
        match_by,
        " is not a valid column name in ",
        data,
        ": Please specify which column should be searched for duplicates"
      ))
    }else{
      data_fd <- data[, match_by]
      if(missing(method)){method <- "string_osa"}
    }
  }

  result <- find_duplicates(data_fd, method = method, ...)
  return(
    extract_unique_references(data, matches = result, type = type)
  )
}

#' Manually review potential duplicates
#' @description Allows users to manually review articles classified as duplicates.
#' @param text A character vector of the text that was used to identify potential duplicates.
#' @param matches Numeric: a vector of group numbers for texts that indicates duplicates and unique values returned by the \code{\link{find_duplicates}} function.
#' @return A \code{data.frame} of potential duplicates grouped together.
review_duplicates <- function(text, matches){
  likely_duplicates <- unique(matches)[table(matches)>1]
  review <- data.frame(
    title = text[matches %in% likely_duplicates],
    matches = matches[matches %in% likely_duplicates],
    stringsAsFactors = FALSE
  )
  review <- review[order(review[,2]),]
  return(review)
}

#' Manually override duplicates
#' @description Re-assign group numbers to text that was classified as duplicated but is unique.
#' @param matches Numeric: a vector of group numbers for texts that indicates duplicates and unique values returned by the \code{\link{find_duplicates}} function.
#' @param overrides Numeric: a vector of group numbers that are not true duplicates.
#' @return The input \code{matches} vector with unique group numbers for members of groups that the user overrides.
override_duplicates <- function(matches, overrides){
  matches <- as.numeric(matches)
  for(i in 1:length(overrides)){
    matches[max(which(matches==overrides[i]))] <- max(matches)+1
  }
  return(matches)
}
