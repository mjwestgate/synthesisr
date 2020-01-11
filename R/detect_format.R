#' Detects if a file is bib-like or ris-like
#'
#' @description Because bibliographic data  can be stored in multiple file types, this function determines if the format of text is one of several likely probabilities: comma-separated; tab-separated; bibtex; and ris (medline or generic).
#' @param x A character vector containing bibliographic data.
#' @return Returns the format of a file: either bib, ris, or unknown.
#' @example inst/examples/bibvris.R
detect_format <- function(x){

  # internal function to calculate the proportion of lines that contain a particular regex
  # called by detect_format
  proportion_delimited <- function(x, regex){
    delimiter_count <- unlist(lapply(
      gregexpr(regex, x),
      function(a){length(which(a > 0))}
    ))
    full_lines <- nchar(x) > 0
    proportion <- length(which(delimiter_count > 0)) / length(which(full_lines))
    return(proportion)
  }

  # calculate proportional of lines containing likely tags
  proportions <- unlist(lapply(
    c(",\"", "\t", "\\{", "^[[:upper:]]{2,}"),
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


#' Detect delimiter type in bibliographic files
#'
#' @description The delimiter in ris files is often an endrow, a special character, or a space. This function detects which delimiter, if any, a file uses.
#' @param x A character vector containing bibliographic data.
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
