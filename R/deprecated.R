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
