#' Write a .bib file
#'
#' @description This is a subfunction of write_bibliography to convert an object of class bibliography to .bib text format.
#' @param x An object of class bibliography.
#' @return Returns a character vector containing references in .bib format.
#' @example inst/examples/parse_ris.R
write_bib <- function(x){
  # process basic text
  result <- lapply(x, function(a){
    if(any(names(a) == "author")){
      a$author <- paste(a$author, collapse=" and ")
    }
    a <- lapply(a, function(b){ 	# ensure only one entry per value
      if(length(b) > 1){
        paste(b, collapse = "; ")
      }else{
        b
      }
    })
    paste0(names(a), "={", a, "},") # format as text
  })

  # add article identifier info
  export <- unlist(lapply(
    seq_len(length(result)),
    function(a, source, entry_names){
      c(
        paste0("@ARTICLE{", entry_names[a], ","),
        source[a],
        "}",
        ""
      )
    },
    source = result,
    entry_names = names(x))
  )
  names(export) <- NULL
  return(export)

}


#' Write a .ris file
#'
#' @description This is a subfunction of write_bibliography to convert an object of class bibliography to .ris text format.
#' @param x An object of class bibliography.
#' @return Returns a character vector containing references in .ris format.
#' @example inst/examples/parse_ris.R
write_ris <- function(x){

  result <- lapply(x, function(a, lookup){
    # convert to tagged vector
    b <- do.call(c, a)
    b <- data.frame(
      tag = c(names(b), "end"),
      entry = c(b, ""),
      stringsAsFactors = FALSE
    )
    rownames(b) <- NULL
    b$tag <- gsub("[[:digit:]]", "", b$tag)


    # page information needs to be treated separately
    if(any(b$tag == "pages")){
      page.row <- which(b$tag == "pages")
      page.sep <- strsplit(b$entry[page.row], "-")[[1]]
      if(length(page.sep) > 1){
        new.rows <- data.frame(
          tag = c("startpage", "endpage"),
          entry = page.sep,
          stringsAsFactors = FALSE
        )
        b <- as.data.frame(rbind(
          b[c(1:(page.row-1)), ],
          new.rows,
          b[c((page.row+1):nrow(b)), ])
        )
      }}
  #  b$order <- seq_len(nrow(b))

    # substitute tags for ris format versions
    b <- merge(lookup, b,
               by.x = "field",
               by.y = "tag",
               all.x = FALSE,
               all.y = TRUE
    )
  #  b <- b[order(b$order), 2:3]
    b <- b[which(!is.na(b$code)), ]

    # concatenate rows, return a vector of strings
    c(paste(b$code, b$entry, sep = "  - "), "")

  },
  lookup = synthesisr::code_lookup[, c(1,3:4)]
  )

  export <- do.call(c, result)
  return(export)
}

#' Export data to a bibliographic format
#'
#' @description This function exports data.frames containing bibliographic information to either a .ris or .bib file.
#' @param x Either a data.frame containing bibliographic information or an object of class bibliography.
#' @param format What format should the data be exported as? Options are ris or bib.
#' @param write_file If TRUE, saves the result to a text file in the working directory.
#' @param filename If write_file is TRUE, the name of the file to be written.
#' @return Returns a character vector containing bibliographic information in the specified format if write_file is FALSE, or saves output to a file if write_file is TRUE.
#' @example inst/examples/parse_ris.R
write_refs <- function(x, format = "ris", write_file=FALSE, filename=NULL){

if(write_file==TRUE){
  if(missing(filename)){
    stop("argument 'filename' is missing, with no default")
  }
}
  if(!any(c("bibliography", "data.frame") == class(x))){
    stop("write_bibliography only accepts objects of class 'data.frame' or 'bibliography'")
  }
  if(class(x) == "data.frame"){
    x <- as.bibliography(x)
  }

  if(format == "bib"){
export <- synthesisr::write_bib(x)

  }

  if(format == "ris"){
export <- synthesisr::write_ris(x)
  	}

if(write_file==TRUE){
  write.table(
    export,
    filename,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE)
}else{return(export)}

}
