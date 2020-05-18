# internal function to calculate the proportion of lines that contain a particular regex
# called by detect_parser
proportion_delimited <- function(x, regex){
  delimiter_count <- unlist(lapply(
    gregexpr(regex, x, perl = TRUE),
    function(a){length(which(a > 0))}
  ))
  full_lines <- nchar(x, type = "bytes") > 0
  proportion <- length(which(delimiter_count > 0)) / length(which(full_lines))
  return(proportion)
}


#' @rdname detect_
detect_parser <- function(x){

  # calculate proportional of lines containing likely tags
  proportions <- unlist(lapply(
    c(
      ",(\"|[[:alnum:]])",
      "\t",
      "\\{|\\}",
      "(^[[:upper:]]{2,4}\\s*(-|:)\\s)|(^([[:upper:]]{2}|[[:upper:]][[:digit:]])\\s*(-|:){0,2}\\s*)"
    ),
    function(a, z){proportion_delimited(z, a)},
    z = x
  ))

  # if any are detection, pick the most likely one
  if(any(proportions > 0.2)){
    result <- switch(
      c("comma", "tab", "bibtex", "ris")[which.max(proportions)],
      "comma" = "parse_csv",
      "tab" = "parse_tsv",
      "bibtex" = "parse_bibtex",
      "ris" = {
        if(length(which(grepl("PMID", x))) > 0){
          "parse_pubmed"
        }else{
          "parse_ris"
        }
      }
    )
  }else{
    result <- "unknown"
  }
  return(result)
}


#' @rdname detect_
detect_delimiter <- function(x){
  if(any(grepl("^ER", x))){
    delimiter <- "endrow"
  }else{
    # special break: same character repeated >6 times, no other characters
    char_list <- strsplit(x, "")
    char_break_test <- unlist(
      lapply(char_list,
             function(a){length(unique(a)) == 1 & length(a > 6)}
      )
    )
    if(any(char_break_test)){
      delimiter <- "character"
    }else{
      # use space as a ref break (last choice)
      space_break_check <- unlist(lapply(
        char_list,
        function(a){all(a == "" | a == " ")}
      ))
      if(any(space_break_check)){
        delimiter <- "space"
      }else{
        stop("import failed: unknown reference delimiter")
      }
    }
  }
  return(delimiter)
}


#' @rdname detect_
detect_lookup <- function(
  tags # a vector of strings representing ris tags
){
  rows <- which(synthesisr::code_lookup$code %in% tags)
  ris_list <- split(
    synthesisr::code_lookup[rows, grepl("ris_", colnames(synthesisr::code_lookup))],
    synthesisr::code_lookup$code[rows]
  )
  ris_matrix <- do.call(
    rbind,
    lapply(ris_list, function(a){apply(a, 2, any)})
  )
  ris_sums <- apply(ris_matrix, 2, sum)
  best_match <- which.max(ris_sums[-1])
  best_proportion <- ris_sums[best_match + 1] / nrow(ris_matrix)
  generic_proportion <- ris_sums[1] / nrow(ris_matrix)
  # default to ris_generic if everything else is bad
  if(best_proportion < 0.75 & generic_proportion > best_proportion){
    match_df <- synthesisr::code_lookup[synthesisr::code_lookup$ris_generic, ]
  }else{ # i.e. if the 'best' match performs perfectly
    if(best_proportion > 0.99){ # i.e. a perfect match
      match_df <- synthesisr::code_lookup[
        synthesisr::code_lookup[, names(best_match)],

      ]
    }else{ # otherwise use the best choice, then generic to fill gaps
       rows_best <- which(
         synthesisr::code_lookup[, names(best_match)] &
         synthesisr::code_lookup$code %in% names(which(ris_matrix[, names(best_match)]))
       )
       rows_generic <- which(
         synthesisr::code_lookup$ris_generic &
         synthesisr::code_lookup$code %in% names(which(!ris_matrix[, names(best_match)]))
       )
      match_df <- synthesisr::code_lookup[c(rows_best, rows_generic), ]
    }
  }

  return(match_df[, c("code", "order", "field")])
}


# internal function for detect_year
guess_year <- function(x){
  number_lookup <- regexpr("[[:alnum:]]{4}", as.character(x))
  if(any(number_lookup > 0)){
    x <- x[number_lookup > 0]
    result_vec <- unlist(lapply(seq_along(x), function(a){
      substr(x[a], start = number_lookup[a], stop = number_lookup[a] + 3)
    }))
    # return(max(as.numeric(result)))
    result <- names(sort(xtabs(~result_vec), decreasing = TRUE)[1])
    return(result)
  }else{
    return(NA)
  }
}

#' @rdname detect_
detect_year <- function(df){
  if(!inherits(df, "data.frame")){
    stop(print("detect_year expects an object of class data.frame as input"))
  }
  lc_colnames <- tolower(colnames(df))
  dates <- grepl("date", lc_colnames) & !grepl("access", lc_colnames)
  if(any(dates)){
    if(any(colnames(df) == "year")) {
      result <- df$year
    }else{
      result <- rep(NA, nrow(df))
    }
    na_rows <- is.na(result)
    if(any(na_rows)){
      result[na_rows] <- unlist(lapply(
        split(df[na_rows, dates], seq_along(na_rows)),
        guess_year
      ))
    }
  }else{
    result <- rep(NA, nrow(df))
  }
  return(result)
}
