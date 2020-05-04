#' Import bibliographic search results
#'
#' @description Imports common bibliographic reference formats (i.e. .bib, .ris, or .txt).
#' @param filename A path to a filename or vector of filenames containing search results to import.
#' @param replace_tags Either a length-1 character stating how should ris tags be replaced (see details for a list of options), or an object inheriting from class \code{data.frame} containing user-defined replacement tags.
#' @param return_df If TRUE (default), returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates (defaults to FALSE).
#' @details The default for argument \code{replace_tags} is \code{"default"}, which estimates what database has been used for ris tag replacement, then fills any gaps with generic tags. Any tags missing from the database (i.e. \code{code_lookup}) are passed unchanged. Other options are to use tags from Web of Science (\code{"wos"}), Scopus (\code{"scopus"}), Ovid (\code{"ovid"}) or Academic Search Premier (\code{"asp"}). If a \code{data.frame} is given, then it must contain two columns: \code{"code"} listing the original tags in the source document, and \code{"field"} listing the replacement column/tag names. The \code{data.frame} may optionally include a third column named \code{"order"}, which specifies the order of columns in the resulting \code{data.frame}; otherwise this will be taken as the row order. Finally, passing \code{"none"} to \code{replace_tags} suppresses tag replacement.
#' @return Returns a data.frame or list of assembled search results.
#' @example inst/examples/read_refs.R
read_refs <- function(
  filename,
  replace_tags = "default",
  return_df = TRUE,
  verbose = FALSE
){

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
    result_list <- lapply(filename, function(a){
      read_ref(
        filename = a,
        replace_tags = replace_tags,
        return_df = return_df,
        verbose = verbose
      )
    })
    names(result_list) <- filename

    # drop any unrecognized file types
    null_check <- unlist(lapply(result_list, is.null))
    if(any(null_check)){
      result_list <- result_list[-which(null_check)]
    }

    if(return_df){
      result <- merge_columns(result_list)
      result$filename <- unlist(
        lapply(seq_len(length(result_list)),
        function(a, data){
          rep(names(data)[a], nrow(data[[a]]))
        },
        data = result_list
      ))
      return(result)
    }else{
      result <- do.call(c, result_list)
      return(result)
    }

  }else{ # i.e. if only one filename given
    return(
      read_ref(
        filename,
        replace_tags = replace_tags,
        return_df = return_df,
        verbose = verbose
      )
    )
  }
}

#' Internal function called by read_refs for each file
#'
#' @description This is the underlying workhorse function that imports bibliographic files; primarily intended to be called from read_refs.
#' @param filename A path to a filename containing search results to import.
#' @param return_df If TRUE, returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates.
#' @return Returns a data.frame or list of assembled search results.
#' @example inst/examples/read_refs.R
read_ref <- function(
  filename,
  replace_tags = "default",
  return_df = TRUE,
  verbose = FALSE
){
  # error checking for replace tags
  if(inherits(replace_tags, "character")){
    if(!any(c("default", "none", "wos", "scopus", "ovid", "asp") == replace_tags)){
      stop("replace_tags should be one of 'default', 'none', 'wos', 'scopus', 'ovid' or 'asp'.")
    }
  }
  if(inherits(replace_tags, "data.frame")){
    if(any(!(c("code", "field") %in% colnames(replace_tags)))){
      stop("if a data.frame is supplied to replace_tags, it must contain columns 'code' & 'field'.")
    }
  }

  if(verbose){cat(paste0("Reading file ", filename, " ... "))}
  x <- readLines(filename, warn = FALSE)
  parse_function <- detect_format(x[1:min(c(length(x), 200))])

  if(parse_function != "unknown"){

    if(parse_function == "parse_ris"){
      df <- do.call(
        parse_function,
        list(x = x, replace_tags = replace_tags)
      )
    }else{
      df <- do.call(
        parse_function,
        list(x = x)
      )
    }

    if(!inherits(df, "data.frame") & return_df){
      df <- as.data.frame.bibliography(df)
    }

    if(inherits(df, "data.frame")){
      df <- clean_df(df)
    }

    if(verbose){cat("done\n")}

    return(df)

  }else{
    warning(paste("file type not recognised for ", filename, " - skipping"))
  }
}