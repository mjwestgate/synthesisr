#' New import function to combine methods from \code{litsearchr} & \code{revtools}
#' @description This is currently for testing purposes only. Naming and functionality might change.
#' @param filename a path to a filename containing search results to import
#' @param save_dataset if TRUE, saves the full search results to a .csv
#' @param save_directory the path to a directory where search results will be saved if save_dataset is set to TRUE
#' @param verbose if TRUE, prints status updates
#' @return a data frame of assembled search results

import_refs <- function(
  filename,
  return_df = TRUE,
  save_dataset = FALSE, # not coded in yet
  save_directory = "./", # not coded in yet
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
      import_refs_internal(a, df)
    },
    df = return_df
    )
    names(result_list) <- filename
    if(return_df){
      result <- merge_columns(result_list)
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
      import_refs_internal(filename, return_df)
    )
  }
}


# underlying workhorse function

# the first section auto-detects some specialised import formats (xls, xml, csv)
# and imports them using specialised code, as per litsearchr.
# Anything else is treated as plain text and is imported via revtools code.
# Any file that can't be imported as text is detected by tryCatch and skipped.

# Note: there is an open question here of how to import .txt.
# At the moment this is passed to revtools code;
# but it could quite easily be passed as a tab-delimited
# basically as .txt can contain any kind of information there is no right answer

import_refs_internal <- function(
  filename,
  return_df = TRUE,
  verbose = TRUE
	){

  # start by detecting file extension
  file_extension_lookup <- regexpr(".[[:alnum:]]{2,}$", filename)
  file_type <- substr(filename, file_extension_lookup+1, nchar(filename))
  if(file_type == "xlsx"){file_type <- "xls"} # messy but functional!

  if(file_type %in% c("csv", "xls", "xml")){
    df <- switch(file_type,
      "csv" = {df <- utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE)},
      "xml" = {as.character(xml2::read_xml(filename))}, # unclear what format this is in
      "xls" = {
        result <- xlsx::read.xlsx(filename, 1)
        result[] <- lapply(result, function(x){
            if(is.factor(x)){as.character(x)}else{x}
          })
        return(result)
      }
    )
    df <- clean_df(df)
  # if none of the standard methods apply, try importing as bib- or ris- like
  }else{
    z <- tryCatch(
      {
        scan(filename,
          sep = "\t",
          what = "character",
          quote = "",
          quiet = TRUE,
          blank.lines.skip = FALSE
        )
        if(verbose){
          message(paste0(
            "file '",filename, "' imported successfully"
          ))
        }
      },
      warning = function(w){
        if(verbose){
          message(paste0(
            "import of file '", filename, "' failed - skipped"
          ))
        }
        return(NULL)
      },
      error = function(e){
        if(verbose){
          message(paste0(
            "import of file '", filename, "' failed - skipped"
          ))
        }
        return(NULL)
      }
    )
    Encoding(z) <- "latin1"
    z <- gsub("<[[:alnum:]]{2}>", "", z) # remove errors from above process

    # detect whether file is bib-like or ris-like via the most common single characters
    nrows <- min(c(200, length(z)))
    zsub <- z[seq_len(nrows)]
    n_brackets <- length(grep("\\{", zsub))
    n_dashes <- length(grep(" - ", zsub))
    if(n_brackets >  n_dashes){
      df <- read_bib(z)  # simple case - no further work needed
    }else{  #  ris format can be inconsistent; custom code needed
      z_dframe <- prep_ris(z, detect_delimiter(zsub))
      # import appropriate format
      if(any(z_dframe$ris == "PMID")){
        df <- read_medline(z_dframe)
      }else{
        df <- read_ris(z_dframe)
      }
    }
    if(return_df){
      df <- as.data.frame(result)
      df <- clean_df(df)
    }
  }
  return(df)
}