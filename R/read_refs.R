#' Import bibliographic data files
#'
#' Import common bibliographic reference formats  such as `.bib`, `.ris`, or
#' `.txt`.
#' @param filename A path to a filename or vector of filenames containing search
#' results to import.
#' @param tag_naming Either a length-1 character stating how should ris tags be
#' replaced (see details for a list of options), or an object inheriting from
#' class `tibble` containing user-defined replacement tags.
#' @param return_df If `TRUE` (default), returns a `tibble`; if `FALSE`,
#' returns a list.
#' @param verbose If `TRUE`, prints status updates (defaults to `FALSE`).
#' @param locale passed to [vroom::vroom_lines()]
#' @param ... Additional arguments, passed to [vroom::vroom()] or [vroom::vroom_lines()]
#' @details Accepted values for `tag_naming` are:
#' '\itemize{
#'     \item `"best_guess"`: estimate which database has been used for ris tag replacement,
#'     then fill any gaps with generic tags. Any tags missing from [code_lookup] are passed
#'     unchanged.
#'     \item `"wos"` Web of Science tags
#'     \item `"scopus"` Scopus tags
#'     \item `"ovid"` OVID tags
#'     \item `"asp"` Academic Search Premier tags
#'     \item `"none"` Do not rename tags
#'     \item A `tibble` with the following columns: \itemize{
#'        \item `"code"` listing the original tags in the source document
#'        \item `"field"` listing the replacement column/tag names
#'        \item `"order"` (optional) listing the order of columns in the resulting `tibble`
#'        }
#' }
#' @return Returns a `tibble` unless `return_df` is set to `FALSE`, when it returns a `list`.
#' @example inst/examples/read_refs.R
#' @export
read_refs <- function(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE,
  locale = vroom::default_locale(),
  ...
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
      read_ref(filename = a,
               tag_naming = tag_naming,
               return_df = return_df,
               verbose = verbose,
               locale = locale,
               ...)
    })
    names(result_list) <- filename

    # drop any unrecognized file types
    null_check <- unlist(lapply(result_list, is.null))
    if(any(null_check)){
      result_list <- result_list[-which(null_check)]
    }

    if(return_df){
      result_list |>
        add_filename() |>
        safe_bind_rows()
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
        verbose = verbose,
        locale = locale,
        ...
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
#' @return Returns a `tibble` or `list` of assembled search results.
#' @noRd
#' @keywords Internal
read_ref <- function(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE,
  locale = default_locale(),
  ...
){

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
  parse_function <- vroom_lines(filename,
                                n_max = 200,
                                locale = locale) |>
                    detect_parser()

  df <- switch(parse_function,
         "parse_ris" = {
           parse_ris(x = vroom_lines(filename,
                                     locale = locale,
                                     ...),
                     tag_naming = tag_naming)
         },
         "parse_pubmed" = {
           parse_pubmed(x = vroom_lines(filename,
                                        locale = locale,
                                        ...))
         },
         "parse_bibtex" = {
           parse_bibtex(x = vroom_lines(filename,
                                        locale = locale,
                                        ...))
         },
         "parse_csv" = {
           vroom(filename,
                 delim =  ",",
                 locale = locale,
                 ...) |>
           match_columns()
         },
         "parse_tsv" = {
           vroom(filename,
                 delim =  "\t",
                 locale = locale,
                 ...) |>
           match_columns()
         },
         { # aka "unknown"
           NULL
         }
        )

  if(is.null(df)){
    warn(paste("file type not recognised for ", filename, " - skipping"))
    return(NULL)
  }

  # return object in correct format
  # note: the `if` test here is needed because `csv` and `tsv` are already
  # `data.frame`s, whereas all other formats return `bibliography`s
  if(inherits(df, "data.frame")){
    if(!return_df){df <- as.bibliography(df)}
  }else{
    if(return_df){df <- as_tibble(df)}
  }
  if(inherits(df, "data.frame")){
    df <- clean_df(df)
  }
  if(verbose){cat("done\n")}
  return(df)
}

#' Internal function used by csv and tsv imports
#' @description Takes an imported `tibble` and rearranges it to match lookup
#' codes.
#' @param df A data.frame that contains bibliographic information.
#' @return Returns a data.frame rearranged and coded to match standard
#' bibliographic fields, with unrecognized fields appended.
#' @noRd
#' @keywords Internal
#' @example inst/examples/match_columns.R
match_columns <- function(df){
  # figure out which columns match known tags
  rename_lookup <- synthesisr::code_lookup |>
    dplyr::filter(.data[["code"]] %in% colnames(df)) |>
    dplyr::select("code", "field")

  # convert this to a vector
  rename_vector <- rename_lookup$code
  names(rename_vector) <- rename_lookup$field

  # pass to `rename()`
  dplyr::rename(df, !!!rename_vector)
}

#' function to add filename to each tibble in a list
#' @param x a named list
#' @noRd
#' @keywords Internal
add_filename <- function(x){
  x_names <- names(x)
  result <- lapply(x_names, \(a){
    dplyr::mutate(x[[a]], filename = a)
  })
  names(result) <- x_names # useful to preserve names attribute
  result
}

#' function to use `bind_rows()` where some columns may have been converted to
#' lists-cols (but not consistently)
#' @param x a named list
#' @noRd
#' @keywords Internal
safe_bind_rows <- function(x){
  # for each tibble, determine the class and name of each column
  class_df <- lapply(names(x), \(a){
    y <- x[[a]]
    tibble(filename = a,
           column = names(y),
           class = unlist(lapply(y, class)))
  }) |>
    bind_rows()

  # determine which columns have multiple classes
  problem_cols <- class_df |>
    dplyr::group_by(.data$column) |>
    dplyr::summarize(n_classes = dplyr::n_distinct(class)) |>
    dplyr::filter(.data$n_classes > 1) |>
    dplyr::pull("column")

  if(length(problem_cols) > 0){
    # find which cols need to be converted to list, and which files they are in
    correct_cols <- class_df |>
      dplyr::filter(.data$column %in% problem_cols &
                    .data$class != "list")

    # use this to map which columns need updating
    correct_split <- split(correct_cols,
                           seq_len(nrow(correct_cols)))
    for(i in seq_along(correct_split)){
      a <- correct_split[[i]]
      df <- x[[a$filename]]
      df[[a$column]] <- as.list(df[[a$column]])
      x[[a$filename]] <- df
    }

    # bind rows as normal
    bind_rows(x)
  }else{
    bind_rows(x)
  }

}
