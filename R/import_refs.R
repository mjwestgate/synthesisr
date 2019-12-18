#' New import function to combine methods from \code{litsearchr} & \code{revtools}
#' @description This is currently for testing purposes only. Naming and functionality might change.
#' @param filename a path to a filename containing search results to import
#' @param return_df if TRUE, returns a dataframe; if FALSE, returns a list
#' @param verbose if TRUE, prints status updates
#' @return a data frame of assembled search results

import_refs <- function(
  filename,
  return_df = TRUE,
  verbose = TRUE
){

  # the following code is an exact duplicate of 'read_bibliography'
  # It allows import of a single file or multiple files.
  invisible(Sys.setlocale("LC_ALL", "C"))
  on.exit(invisible(Sys.setlocale("LC_ALL", "")))

  if(missing(filename)){
    stop("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if(any(!file_check)){
    stop("file not found")
  }

  if(length(filename) > 1){
    result_list <- lapply(filename, function(a, df){
      synthesisr::import_refs_internal(a, df)
    },
    df = return_df
    )
    names(result_list) <- filename

  # drop any unrecognized file types
  if(any(unlist(lapply(result_list, is.null)))){
    result_list <- result_list[-which(unlist(lapply(result_list, is.null)))]
  }

    if(return_df){
      result <- synthesisr::merge_columns(result_list)
      result$filename <- unlist(
        lapply(seq_len(length(result_list)),
        function(a, data){
          rep(names(data)[a], nrow(data[[a]]))
        },
        data = result_list
      ))
      if(any(colnames(result) == "label")){
        result$label <- make.unique(result$label)
      }
      return(result)
    }else{
      result <- do.call(c, result_list)
      return(result)
    }
  }else{
    return(
      synthesisr::import_refs_internal(filename, return_df)
    )
  }
}


# underlying workhorse function
## love it!! so much better than my clunky for-loops

# the first section auto-detects some specialised import formats (xls, xml, csv)
# and imports them using specialised code, as per litsearchr.
# Anything else is treated as plain text and is imported via revtools code.
# Any file that can't be imported as text is detected by tryCatch and skipped.

## tryCatch led to errors for every plain text file type I tried
## but I think I found a workaround so that shouldn't matter, and i am not too worried
## because the code can recognize files converted in open source software (i.e. zotero)
## for example, I have some .bib that won't read because they are one massive character vector, no breaks
## but I can just convert them because zotero still recognizes them as .bib

# Note: there is an open question here of how to import .txt.
# At the moment this is passed to revtools code;
# but it could quite easily be passed as a tab-delimited
# basically as .txt can contain any kind of information there is no right answer

## how I set it up now is that .txt gets checked to see if it is tab-delim
## if it is, then it gets imported that way and columns are matched to a lookup system
## if it is not, then it gets checked to see if it is bib or ris-like
## if it is nothing sensible, then it just gets ignored

import_refs_internal <- function(
  filename,
  return_df = TRUE,
  verbose = TRUE
	){

  # start by detecting file extension
  ## converted this to a function that also wraps up detecting bib vs ris and ignoring unrecognizable things

  file_type <- synthesisr::detect_filetype(filename)

  ## if it is something obvious, then import right away
  if(file_type=="bib"){
    df <- synthesisr::read_bib(readLines(filename))
    df <- synthesisr::as.data.frame.bibliography(df)

  }

  if(file_type=="txt"){
    df <- read.delim(filename,row.names = NULL)
    df <- synthesisr::match_columns(df)
    df <- synthesisr::clean_df(df)
  }

  ## if it is ris, then clean and prep it first
  if(file_type=="ris"){
    z <- readLines(filename)
    z_dframe <- synthesisr::prep_ris(z, synthesisr::detect_delimiter(z))

    # import appropriate format
    if(any(z_dframe$ris == "PMID")){
      df <- synthesisr::read_medline(z_dframe)
      df <- synthesisr::as.data.frame.bibliography(df)
    }else{
      df <- synthesisr::read_ris(z_dframe)
      df <- synthesisr::as.data.frame.bibliography(df)
    }
  }
if(file_type!="unknown"){
  return(df)
}
}

#' Detects file types
#' @description Given a file, determines the file extension
#' @param file a path to a file
#' @return a character vector with the likely file type based on the file extension
detect_filetype <- function(filename) {
  file_type <- ""
  file_extension_lookup <- regexpr(".[[:alnum:]]{2,}$", filename)
  file_type <-
    substr(filename, file_extension_lookup + 1, nchar(filename))

  if (file_type == "nbib") {
    file_type <- "bib"
  }

  if (!any(c("ris", "bib") == file_type)) {
    if (ncol(read.delim(filename, sep = "\t", row.names = NULL)) == 1) {
      file_type <- synthesisr::bibvris(readLines(filename))
    } else{
      file_type <- "txt"
    }
  }
  if (!any(c("ris", "bib", "txt") == file_type)) {
    file_type <- "unknown"
    print(paste("Note: file type for", filename, "not recognized."))
  }
  return(file_type)
}


bibvris <- function(z){
  nrows <- min(c(200, length(z)))
  n_brackets <- length(grep("\\{", z))
  n_dashes <- length(grep(" - ", z))
  if(n_brackets > nrows/3 | n_dashes>nrows/3){
    if(n_brackets>n_dashes){
      file_type <- "bib"
    }else{file_type <- "ris"}
  }else{file_type <- "unknown"}
  return(file_type)
}


#' Match imported data to reference codes
#' @description Takes an imported dataframe, rearranges it to match lookup codes, and removes redundant columns
#' @param df a data frame that contains bibliographic information
#' @return a data frame rearranged and coded to match standard bibliographic fields, with other fields appended
match_columns <- function(df){
  # Thanks for Martin's code in revtools for the idea to use lookups! ...and some of the tag lookups

  # figure out which columns match known tags
  hits <- as.numeric(match(synthesisr::code_lookup$code, colnames(df)))

  # rearrange data in standard order
  if(any(is.na(hits))){
    newdat <- df[, hits[!is.na(hits)]]
  }else{newdat <- df[,hits]}

  # retain columns even if they did not match lookup
  newcolnames <- synthesisr::code_lookup$field[match(colnames(newdat), synthesisr::code_lookup$code)]
  newcolnames[which(is.na(newcolnames) | newcolnames=="")] <- colnames(newdat)[which(is.na(newcolnames) | newcolnames=="")]
  colnames(newdat) <- newcolnames

  # drop duplicate columns that matched more than one tag
  newdat <- newdat[,-which(duplicated(colnames(newdat)))]

  return(newdat)
}

remove_factors <- function(z){
  z[] <- lapply(z, function(x){
    if(is.factor(x)){as.character(x)}else{x}
  })
  return(z)
}
