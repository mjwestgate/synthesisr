# internal function to calculate the proportion of lines that contain a particular regex
# called by detect_format
proportion_delimited <- function(x, regex){
  delimiter_count <- unlist(lapply(
    gregexpr(regex, x, perl = TRUE),
    function(a){length(which(a > 0))}
  ))
  full_lines <- nchar(x, type = "bytes") > 0
  proportion <- length(which(delimiter_count > 0)) / length(which(full_lines))
  return(proportion)
}


#' Detects if a file is bib-like or ris-like
#'
#' @description Because bibliographic data  can be stored in multiple file types, this function determines if the format of text is one of several likely probabilities: comma-separated; tab-separated; bibtex; and ris (medline or generic).
#' @param x A character vector containing bibliographic data.
#' @return Returns the format of a file: either bib, ris, or unknown.
#' @example inst/examples/detect_format.R
detect_format <- function(x){

  # calculate proportional of lines containing likely tags
  proportions <- unlist(lapply(
    c(",", "\t", "\\{|\\}", "^([[:upper:]]{2,4}|[[:upper:]][[:digit:]])\\s*-{0,2}\\s*"),
    function(a, z){proportion_delimited(z, a)},
    z = x
  ))
  # NOTE: if we use 'comma not followed by space' as our regex,
  # we can use switch/which.max for choosing the 'best' delimiter

  # if any are detection, pick the most likely one
  if(any(proportions[2:4] > 0.1)){
    result <- switch(
      c("tab", "bibtex", "ris")[which.max(proportions[2:4])],
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
    if(proportions[1] > 0.2){
      result <- "parse_csv"
    }else{
      result <- "unknown"
    }
  }
  return(result)
}


#' Detect delimiter type in bibliographic files
#'
#' @description The delimiter in ris files is often an endrow, a special character, or a space. This function detects which delimiter, if any, a file uses.
#' @param x A character vector containing RIS-formatted bibliographic data.
#' @return Returns the delimiter type used in a file.
#' @example inst/examples/detect_delimiter.R
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

# Function to ensure that ris tags are matched correctly
# tags a vector of strings
# and returns a subsetted version of code_lookup that contains only useful information
detect_tags <- function(
  tags # a vector of strings representing ris tags
){
  rows <- which(code_lookup$code %in% tags)
  ris_list <- split(
    code_lookup[rows, grepl("ris_", colnames(code_lookup))],
    code_lookup$code[rows]
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
    match_df <- code_lookup[
      code_lookup$ris_generic,
      c("code", "order", "field")
    ]
  }else{ # i.e. if the 'best' match performs perfectly
    if(best_proportion > 0.99){ # i.e. a perfect match
      match_df <- code_lookup[
        code_lookup[, names(best_match)],
        c("code", "order", "field")
      ]
    }else{ # otherwise use the best choice, then generic to fill gaps
       rows_best <- which(
         code_lookup[, names(best_match)] &
         code_lookup$code %in% names(which(ris_matrix[, names(best_match)]))
       )
       rows_generic <- which(
         code_lookup$ris_generic &
         code_lookup$code %in% names(which(!ris_matrix[, names(best_match)]))
       )
      match_df <- code_lookup[c(rows_best, rows_generic), ]
    }
  }

  return(match_df)
}


#' Fill in probable year values
#' @description Given a data.frame containing bibliographic data, this function attempts to detect the publication year based on date fields for articles missing a year.
#' @param df a data.frame containing bibliographic data
#' @return the input data.frame with years added for articles missing that information
#' @example inst/examples/detect_year.R
detect_year <- function(df){
  if(!inherits(df, "data.frame")){
    stop(print("detect_year expects an object of class data.frame as input"))
  }
  dates <- grep("date", tolower(colnames(df)))
  if(any(grep("access", tolower(colnames(df))[dates]))){
    dates <- dates[-grep("access", tolower(colnames(df)[dates]))]
    }

  guess_year <- function(x) {
    possible_years <- strsplit(gsub("[[:punct:]]", " ", paste(x[dates], collapse = " ")), " ")[[1]]
    possible_years <- suppressWarnings(possible_years[which(nchar(possible_years) == 4 &
                                                              !is.na(as.numeric(possible_years)))])
    if (any(!is.na(possible_years))) {
      names(sort(table(possible_years), decreasing = TRUE))[1]
    } else{
      "<NA>"
    }
  }

  if(any(dates)){
    if (any(colnames(df) == "year")) {
      need_years <- which(is.na(as.numeric(df$year)))
    }else{
      need_years <- seq(1, nrow(df), 1)
      df$year <- rep("<NA>", nrow(df))
    }
    df[need_years, 'year'] <- apply(df[need_years,],1, guess_year)
    }

  return(df)
}