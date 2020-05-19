# Parse an object of class bibliography for export in bib format
#' @describeIn write_refs Format a bib file for export
write_bib <- function(x) {
  # process basic text
  result <- lapply(x, function(a) {
    if (any(names(a) == "author")) {
      a$author <- paste(a$author, collapse = " and ")
    }
    a <- lapply(a, function(b) {
      # ensure only one entry per value
      if (length(b) > 1) {
        paste(b, collapse = "; ")
      } else{
        b
      }
    })
    paste0(names(a), "={", a, "},") # format as text
  })

  # add article identifier info
  export <- unlist(
    lapply(seq_len(length(result)),
    function(a, source, entry_names) {
      c(paste0("@ARTICLE{", entry_names[a], ","),
        source[a],
        "}",
        "")
    },
    source = result,
    entry_names = names(x)))
  names(export) <- NULL
  return(export)

}


# Parse an object of class bibliography for export in ris format
#' @describeIn write_refs Format a ris file for export
write_ris <- function(x,
  tag_naming = "synthesisr"
){
  result <- lapply(x, function(a, lookup) {

    # convert to tagged vector
    b <- do.call(c, a)
    b <- b[!is.na(b)]
    b <- data.frame(
      tag = c(names(b), "end"),
      entry = c(b, ""),
      stringsAsFactors = FALSE
    )
    rownames(b) <- NULL
    b$tag <- gsub("[[:digit:]]", "", b$tag)

    # page information needs to be treated separately
    if(any(b$tag == "pages")){
      page_row <- which(b$tag == "pages")
      page_text <- b$entry[page_row]
      if(grepl("-", page_text)){
        text_lookup <- list(
          regexpr("^[[:digit:]]+", page_text),
          regexpr("-[[:digit:]]+", page_text)
        )
        if(all(text_lookup > 0)){
          text_cleaned <- unlist(lapply(
            text_lookup,
            function(b){substr(page_text, b, b + attr(b, "match.length") - 1)}
          ))
          new_rows <- data.frame(
            tag = c("startpage", "endpage"),
            entry = gsub("[[:punct:]]", "", text_cleaned),
            stringsAsFactors = FALSE
          )
          b <- as.data.frame(rbind(
            b[c(1:(page_row - 1)),],
            new_rows,
            b[c((page_row + 1):nrow(b)),]
          ))
        }
      }
    }
    b$order <- seq_len(nrow(b))

    # substitute tags for ris format versions
    b <- merge(
      lookup,
      b,
      by.x = "field",
      by.y = "tag",
      all.x = FALSE,
      all.y = FALSE
    )
    b <- b[order(b$order), c(2:3)]

    # concatenate rows, return a vector of strings
    return(
      c(paste(b$code, b$entry, sep = "  - "), "ER  - ", "")
    )

  },
  lookup = synthesisr::code_lookup[
    synthesisr::code_lookup[, paste0("ris_", tag_naming)],
    c("code", "field")
  ]
  )

  export <- do.call(c, result)
  return(export)
}

#' Export data to a bibliographic format
#'
#' @description This function exports data.frames containing bibliographic information to either a .ris or .bib file.
#' @param x Either a data.frame containing bibliographic information or an object of class bibliography.
#' @param format What format should the data be exported as? Options are ris or bib.
#' @param tag_naming what naming convention should be used to write RIS files? See details for options.
#' @param file Either logical indicating whether a file should be written (defaulting to FALSE), or a character giving the name of the file to be written.
#' @return Returns a character vector containing bibliographic information in the specified format if \code{file} is FALSE, or saves output to a file if TRUE.
#' @example inst/examples/parse_.R
write_refs <- function(
  x,
  format = "ris",
  tag_naming = "synthesisr",
  file = FALSE # either logical or a character (i.e. a file name)
){
  # check input data
  if(!any(c("bibliography", "data.frame") == class(x))) {
    stop("write_bibliography only accepts objects of class 'data.frame' or 'bibliography'")
  }
  if(inherits(x, "data.frame")){
    x <- as.bibliography(x)
  }

  # check format
  if(!(format %in% c("ris", "bib"))){
    stop("format must be either 'ris' or 'bib'")
  }

  # check output format - consistent with read_refs
  if(format == "ris"){
    valid_tags <- c("best_guess", "none", "wos", "scopus", "ovid", "asp", "synthesisr")
    if(inherits(tag_naming, "character")){
      if(!any(valid_tags == tag_naming)){
        stop("tag_naming should be one of 'best_guess', 'none', 'wos', 'scopus', 'ovid',  'asp' or 'synthesisr'.")
      }
    }else if(inherits(tag_naming, "data.frame")){
      if(any(!(c("code", "field") %in% colnames(tag_naming)))){
        stop("if a data.frame is supplied to replace_tags, it must contain columns 'code' & 'field'.")
      }
    }
  }

  # check file information
  if(length(file) > 1){
    stop("argument 'file' should be a length-1 character or logical")
  }
  if(!inherits(file, c("logical", "character"))){
    stop("argument 'file' should be either logical or character")
  }
  if(inherits(file, "character")){
    file_out <- TRUE
    if(grepl("\\.[[:alpha:]]{2,4}$", file)){
      filename <- file
    }else{
      filename <- paste(file, format, sep = ".")
    }
  }else{ # i.e. logical
    if(file){
      file_out <- TRUE
      filename <- paste("synthesisr_bibliography", format, sep = ".")
    }else{
      file_out <- FALSE
    }
  }

  # write result in correct format
  export <- switch(format,
    "bib" = {write_bib(x)},
    "ris" = {write_ris(x, tag_naming = tag_naming)}
  )

  if(file_out) {
    write.table(
      export,
      filename,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
  }else{
    invisible(return(export))
  }
}
