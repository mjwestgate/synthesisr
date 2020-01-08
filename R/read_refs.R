#' Import bibliographic search results
#'
#' @description Imports common bibliographic reference formats (i.e. .bib, .ris, or .txt).
#' @param filename A path to a filename or vector of filenames containing search results to import.
#' @param return_df If TRUE, returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates.
#' @return Returns a data.frame or list of assembled search results.
#' @example inst/examples/import_refs.R
read_refs <- function(
  filename,
  return_df = TRUE,
  verbose = TRUE
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
    result_list <- lapply(filename, function(a, df){
      synthesisr::read_ref(a, df)
    },
    df = return_df
    )
    names(result_list) <- filename

    # drop any unrecognized file types
    null_check <- unlist(lapply(result_list, is.null))
    if(any(null_check)){
      result_list <- result_list[-which(null_check)]
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

  }else{ # i.e. if onely one filename given
    return(
      synthesisr::read_ref(filename, return_df)
    )
  }
}

#' Internal function called by import_refs for each file
#'
#' @description This is the underlying workhorse function that imports bibliographic files; primarily intended to be called from import_refs.
#' @param filename A path to a filename containing search results to import.
#' @param return_df If TRUE, returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates.
#' @return Returns a data.frame or list of assembled search results.
#' @example inst/examples/import_refs.R
read_ref <- function(
  filename,
  return_df = TRUE,
  verbose = TRUE
	){

  if(verbose){cat(paste0("Reading file ", filename, " ... "))}
  x <- readLines(filename, warn = FALSE)

  parse_function <- detect_format(x[1:min(c(length(x), 200))])
  
  if(parse_function != "unknown"){

    x <- readLines(filename, warn = FALSE)
    df <- do.call(parse_function, list(x = x))

    if(!inherits(df, "data.frame") & return_df){
      df <- as.data.frame(df)
      df <- synthesisr::clean_df(df)
    }

    if(verbose){cat("done\n")}

    return(df)
  }else{
    warning(paste("file type not recognised for ", filename, " - skipping"))
  }

}