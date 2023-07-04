# internal function used by parse_csv and parse_tsv:

#' Matches imported data to reference codes
#'
#' @description Takes an imported data.frame and rearranges it to match lookup
#' codes.
#' @param df A data.frame that contains bibliographic information.
#' @return Returns a data.frame rearranged and coded to match standard
#' bibliographic fields, with unrecognized fields appended.
#' @noRd
#' @keywords Internal
#' @example inst/examples/match_columns.R
match_columns <- function(df){
  # figure out which columns match known tags
  hits <- as.numeric(match(synthesisr::code_lookup$code, colnames(df)))
  newcolnames <- synthesisr::code_lookup$field[
    match(colnames(df),
    synthesisr::code_lookup$code)
  ]
  colnames(df)[!is.na(newcolnames)] <- newcolnames[!is.na(newcolnames)]

  # rearrange data in standard(ish) order
  if(any(is.na(hits))){
    hits <- hits[!is.na(hits)]
  }

  # retain columns even if they did not match lookup
  retain <- append(hits, seq(1, length(df), 1)[!(seq(1, length(df), 1) %in% hits)])

  return(df[,retain])
}

#' Bind two or more data frames with different columns
#'
#' @description Takes two or more `data.frames` with different column names or
#' different column orders and binds them to a single `data.frame.` This
#' function is maintained for backwards compatibility, but it is synonymous with
#' `dplyr::bind_rows()` and will be depracated in future.
#' @param x Either a data.frame or a list of data.frames.
#' @param y A data.frame, optional if x is a list.
#' @return Returns a single data.frame with all the input data frames merged.
#' @example inst/examples/merge_columns.R
#' @importFrom dplyr bind_rows
#' @importFrom rlang abort
#' @export
merge_columns <- function(
  x, # either a data.frame or a list of the same
  y # a data.frame, optional
){
  if(missing(x)){
    abort("object x is missing with no default")
  }
  if(!(inherits(x, "data.frame") | inherits(x, "list"))){
    abort("object x must be either a data.frame or a list")
  }
  if(inherits(x, "data.frame")){
    if(missing(y)){
      return(x)
      # abort("If x is a data.frame, then y must be supplied")
    }else{
      x <- list(x, y)
    }
  }else{ # i.e. for lists
    if(!all(unlist(lapply(x, function(a){inherits(a, "data.frame")})))){
      abort("x must only contain data.frames")
    }
  }
  bind_rows(x)
}
