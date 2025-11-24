#' Export data to a bibliographic format
#'
#' @description This function exports data.frames containing bibliographic
#' information to either a .ris or .bib file.
#' @param x Either a data.frame containing bibliographic information or an
#' object of class bibliography.
#' @param file filename to save to.
#' @param format What format should the data be exported as? Options are ris or
#' bib.
#' @param tag_naming what naming convention should be used to write RIS files?
#' See details for options.
#' @param write Logical should a file should be written? If FALSE returns a
#' `list`.
#' @return This function is typically called for it's side effect of writing a
#' file in the specified location and format. If \code{write} is FALSE, returns
#' a character vector containing bibliographic information in the specified
#' format.
#' @example inst/examples/parse_.R
#' @rdname write_refs
#' @export
write_refs <- function(
    x,
    file,
    format = "ris",
    tag_naming = "synthesisr",
    write = TRUE
){
  # check input data
  if(!inherits(x, c("bibliography", "data.frame"))) {
    abort("write_bibliography only accepts objects of class 'data.frame' or 'bibliography'")
  }
  if(inherits(x, "data.frame")){
    x <- x |>
      as.data.frame() |>
      as.bibliography()
  }

  if(missing(file) & (write == TRUE)){
    abort("`file` is missing, with no default")
  }

  # check format
  if(!(format %in% c("ris", "bib"))){
    abort("format must be either 'ris' or 'bib'")
  }

  # check output format - consistent with read_refs
  if(format == "ris"){
    valid_tags <- c("best_guess", "none", "wos", "scopus", "ovid", "asp", "synthesisr")
    if(inherits(tag_naming, "character")){
      if(!any(valid_tags == tag_naming)){
        abort("tag_naming should be one of 'best_guess', 'none', 'wos', 'scopus', 'ovid',  'asp' or 'synthesisr'.")
      }
    }else if(inherits(tag_naming, "data.frame")){
      if(any(!(c("code", "field") %in% colnames(tag_naming)))){
        abort("if a data.frame is supplied to replace_tags, it must contain columns 'code' & 'field'.")
      }
    }
  }

  # write result in correct format
  export <- switch(format,
                   "bib" = {write_bib(x)},
                   "ris" = {write_ris(x, tag_naming = tag_naming)}
  )
  names(export) <- NULL

  if(write) {
    write.table(
      export,
      check_filename(file),
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
  }else{
    invisible(return(export))
  }
}

#' Internal function to check file names
#' @noRd
#' @keywords Internal
check_filename <- function(x){
  # check file information
  if(length(x) > 1){
    abort("argument 'file' should be a length-1 character")
  }
  if(!inherits(x, "character")){
    abort("argument 'file' should be an object of class `character`")
  }
  if(grepl("\\.[[:alpha:]]{2,4}$", x)){
    filename <- x
  }else{
    filename <- paste(x, format, sep = ".")
  }
  filename
}


# Parse an object of class bibliography for export in bib format
#' @rdname write_refs
#' @export
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
#' @rdname write_refs
#' @export
write_ris <- function(x,
  tag_naming = "synthesisr"
){
  # first get the tags we want to use here
  tag_column <- glue::glue("ris_{tag_naming}")
  lookup_tibble <- synthesisr::code_lookup |>
    dplyr::filter(.data[[tag_column]] == TRUE) |>
    dplyr::select("code", "field")

  # then convert to ris, one entry at a time
  result <- lapply(x, function(a, lookup) {

    # convert to tagged vector
    b <- do.call(c, a)
    b <- b[!is.na(b)]
    b <- tibble(tag = c(names(b), "end"),
                entry = c(b, ""))
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
          b <- as_tibble(rbind(
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
  lookup = lookup_tibble)
  do.call(c, result)
}
