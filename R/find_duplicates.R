#' Detect duplicate values
#'
#' @description Identifies duplicate bibliographic entries using different duplicate detection methods.
#' @param data A data.frame that contains duplicate bibliographic entries.
#' @param match_variable A length-1 integer or string listing the column in which duplicates should be sought. Defaults to doi where available, followed by title. If neither are found the function will fail.
#' @param group_vars An optional vector listing the columns to use as grouping variables; that is, categories withing which duplicates should be sought (see 'note'). Optionally NULL to compare all entries against one another.
#' @param match_function The duplicate detection method to use; options are "stringdist" for similarity, "fuzzdist" for fuzzy matching, or "exact" for exact matches
#' @param method A string indicating the method to use for fuzzdist.
#' @param threshold Numeric: the cutoff threshold for stringdist or fuzzdist.
#' @param to_lower Logical: Should all entries should be considered in lowercase when detecting duplicates?
#' @param rm_punctuation Logical: Should punctuation should be removed when detecting duplicates?
#' @return Returns a vector of duplicate matches and methods used.
#' @example inst/examples/deduplicate.R
find_duplicates <- function(
  data,
  match_variable,
  group_vars  = NULL,
  match_function  = c("stringdist",  "fuzzdist", "exact"),
  method,
  threshold  = 5,
  to_lower = FALSE,
  rm_punctuation = FALSE
){

  if(match_function=="stringdist"){
    if(!requireNamespace("stringdist")){
      match_function <- "fuzzdist"
      print("Note: stringdist must be installed to use stringdist method. Using fuzzdist instead.")
    }else{requireNamespace("stringdist")}
  }

  # error catching
  # data
  if(missing(data)){
    stop("'data' is missing: Please provide a data.frame")
  }
  if(missing(group_vars)){
    group_vars <- NULL
  }else{
    if(!all(group_vars %in% colnames(data))){
      group_vars <- NULL
    }
  }

  # match variable
  if(missing(match_variable)){
    if(any(colnames(data) == "doi")){
      match_variable <- "doi"
      if(missing(match_function)){match_function <- "exact"}
    }else{
      if(any(colnames(data) == "title")){
        match_variable <- "title"
      }else{
        stop("match_variable is missing, with no default;
          please specify which column should be searched for duplicates"
        )
      }
    }
  }else{
    if(!any(colnames(data) == match_variable)){
      stop(paste0(
        match_variable,
        " is not a valid column name in ",
        data,
        ": Please specify which column should be searched for duplicates"
      ))
    }
  }

  # methods
  if(missing(match_function)){match_function <- "exact"}
  if(!any(c("fuzzdist", "stringdist", "exact") == match_function)){
    stop(
      paste0(
        match_function,
        " is an invalid input to match_function; please specify one of 'fuzzdist', 'stringdist' or 'exact'."
      )
    )
  }
  if(missing(method)){
    if(match_function == "stringdist"){method <- "osa"}
    if(match_function == "fuzzdist"){method <- "fuzz_m_ratio"}
  }
  if(missing(threshold)){
    if(match_function == "stringdist"){threshold <- 5}
    if(match_function == "fuzzdist"){threshold <- 0.1}
  }
  if(match_function == "exact"){
    method <- NA
    threshold <- NA
  }else{
    valid_methods <- eval(formals(match_function)$method)
    if(!any(valid_methods == method)){
      stop(paste0("'",
        method,
        "' is not a valid method for function '",
        match_function,
        "'; Please specify one of the following arguments: '",
        paste(valid_methods, collapse = "', '"),
        "'"
      ))
    }
  }

  # prep columns
  if(any(colnames(data) == "checked") == FALSE){
    data$checked <- FALSE
  }
  if(any(colnames(data) == "group") == FALSE){
    data$group <- NA
  }
  if(to_lower){
    data[, match_variable] <- tolower(data[, match_variable])
  }
  if(rm_punctuation){
    data[, match_variable] <- synthesisr::remove_punctuation(data[, match_variable])
  }

  # run while loop
  progress <- 1
	while(all(data$checked) == FALSE){
    remaining_rows <- which(data$checked == FALSE)
    if(length(remaining_rows) == 1){
			data$group[remaining_rows] <- progress
		  data$checked[remaining_rows] <- TRUE
		}else{
      # locate relevant information
			row_start <- remaining_rows[1]
      # if this entry is empty, then skip (i.e. never match NAs)
      if(is.na(data[row_start, match_variable])){
        data$checked[row_start] <- TRUE
        data$group[row_start] <- progress
      }else{
        # include only those entries in the same grouping categories as the current entry
        # plus any entries that are missing those values
        if(is.null(group_vars)){
          rows_tr <- remaining_rows
        }else{
          match_list <- lapply(group_vars, function(a, data, row){
            (data[, a] == data[row_start, a]) | is.na(data[, a])
            },
            data = data,
            row = row_start
          )
          if(length(group_vars) == 1){
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
          if(match_function == "exact"){
            match_result <- (data[rows_tr, match_variable] == data[row_start, match_variable])
          }else{
            match_result <- do.call(
              match_function,
              list(
                a = data[row_start, match_variable],
                b = data[rows_tr, match_variable],
                method = method
              )
            ) <= threshold
          }
          if(any(match_result, na.rm = TRUE)){
            rows_selected <- rows_tr[which(match_result)]
            data$checked[c(row_start, rows_selected)] <- TRUE
            data$group[c(row_start, rows_selected)] <- progress
          }else{
            data$checked[row_start] <- TRUE
            data$group[row_start] <- progress
          }
        }else{
          data$checked[row_start] <- TRUE
          data$group[row_start] <- progress
        }
      } # end if(is.na(data[row_start, match_variable]))
    } # end if(length(remaining_rows) == 1)
    progress <- progress + 1
  } # end while loop

  # add attributes
  result <- data$group
  attr(result, "match_variable") <- match_variable
  if(is.null(group_vars)){
    attr(result, "group_vars") <- NA
  }else{
    attr(result, "group_vars") <- group_vars
  }
  attr(result, "match_function") <- match_function
  attr(result, "method") <- method
  attr(result, "threshold") <- threshold
  return(result)
}
