#' Import bibliographic search results
#'
#' @description Imports common bibliographic reference formats (i.e. .bib, .ris,
#' or .txt).
#' @param filename A path to a filename or vector of filenames containing search
#' results to import.
#' @param tag_naming Either a length-1 character stating how should ris tags be
#' replaced (see details for a list of options), or an object inheriting from
#' class `data.frame` containing user-defined replacement tags.
#' @param return_df If `TRUE` (default), returns a `data.frame`; if `FALSE`,
#' returns a list.
#' @param verbose If `TRUE`, prints status updates (defaults to `FALSE`).
#' @details The default for argument `tag_naming` is `"best_guess"`,
#' which estimates what database has been used for ris tag replacement, then
#' fills any gaps with generic tags. Any tags missing from the database (i.e.
#' `code_lookup`) are passed unchanged. Other options are to use tags from
#' Web of Science (`"wos"`), Scopus (`"scopus"`), Ovid (`"ovid"`)
#' or Academic Search Premier (`"asp"`). If a `data.frame` is given,
#' then it must contain two columns: `"code"` listing the original tags in
#' the source document, and `"field"` listing the replacement column/tag
#' names. The `data.frame` may optionally include a third column named
#' `"order"`, which specifies the order of columns in the resulting
#' `data.frame`; otherwise this will be taken as the row order. Finally,
#' passing `"none"` to `replace_tags` suppresses tag replacement.
#' @return Returns a `data.frame` or `list` of assembled search results.
#' @importFrom rlang abort
#' @example inst/examples/read_refs.R
#' @export
read_refs <- function(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE
){

  if(missing(filename)){
    abort("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if(any(!file_check)){
    abort("file not found")
  }

  if(length(filename) > 1){
    result_list <- lapply(filename, function(a){
      read_ref(
        filename = a,
        tag_naming = tag_naming,
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
        tag_naming = tag_naming,
        return_df = return_df,
        verbose = verbose
      )
    )
  }
}

#' Internal function called by read_refs for each file
#'
#' @description This is the underlying workhorse function that imports
#' bibliographic files; primarily intended to be called from read_refs.
#' @param filename A path to a filename containing search results to import.
#' @param return_df If TRUE, returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates.
#' @return Returns a data.frame or list of assembled search results.
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @noRd
#' @keywords Internal
read_ref <- function(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE
){
  invisible(Sys.setlocale("LC_ALL", "C"))
  on.exit(invisible(Sys.setlocale("LC_ALL", "")))

  # error checking for replace tags
  valid_tags <- c("best_guess", "none", "wos", "scopus", "ovid", "asp", "synthesisr")
  if(inherits(tag_naming, "character")){
    if(!any(valid_tags == tag_naming)){
      abort("tag_naming should be one of 'best_guess', 'none', 'wos', 'scopus', 'ovid',  'asp' or 'synthesisr'.")
    }
  }
  if(inherits(tag_naming, "data.frame")){
    if(any(!(c("code", "field") %in% colnames(tag_naming)))){
      abort("if a data.frame is supplied to replace_tags, it must contain columns 'code' & 'field'.")
    }
  }

  if(verbose){cat(paste0("Reading file ", filename, " ... "))}
  x <- readLines(filename, warn = FALSE)
  parse_function <- detect_parser(x[1:min(c(length(x), 200))])

  if(parse_function != "unknown"){

    # parse correctly
    if(parse_function == "parse_ris"){
      df <- do.call(
        parse_function,
        list(x = x, tag_naming = tag_naming)
      )
    }else{
      df <- do.call(parse_function, list(x = x))
    }

    # return object in correct format
    if(inherits(df, "data.frame")){
      if(!return_df){df <- as.bibliography(df)}
    }else{
      if(return_df){df <- as.data.frame.bibliography(df)}
    }
    if(inherits(df, "data.frame")){df <- clean_df(df)}
    if(verbose){cat("done\n")}
    return(df)

  }else{
    warn(paste("file type not recognised for ", filename, " - skipping"))
  }
}
