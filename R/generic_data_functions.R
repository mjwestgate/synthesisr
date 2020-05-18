# internal function used by parse_csv and parse_tsv
# ' Matches imported data to reference codes
# '
# ' @description Takes an imported data.frame and rearranges it to match lookup codes.
# ' @param df A data.frame that contains bibliographic information.
# ' @return Returns a data.frame rearranged and coded to match standard bibliographic fields, with unrecognized fields appended.
# ' @example inst/examples/match_columns.R
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
#' @description Takes two or more data.frames with different column names or different column orders and binds them to a single data.frame.
#' @param x Either a data.frame or a list of data.frames.
#' @param y A data.frame, optional if x is a list.
#' @return Returns a single data.frame with all the input data frames merged.
#' @example inst/examples/merge_columns.R
merge_columns <- function(
  x, # either a data.frame or a list of the same
  y # a data.frame, optional
){
  if(missing(x)){
    stop("object x is missing with no default")
  }

  if(!any(c("data.frame", "list") == class(x))){
    stop("object x must be either a data.frame or a list")
  }

  if(class(x) == "data.frame"){
    if(missing(y)){
      stop("If x is a data.frame, then y must be supplied")
    }
    x <- list(x, y)
  }else{ # i.e. for lists
    if(!all(unlist(lapply(x, class)) == "data.frame")){
      stop("x must only contain data.frames")
    }
  }

  x <- lapply(x, remove_factors)

  col_names_all <- unique(unlist(lapply(x, colnames)))

  result_list <- lapply(x, function(a, cn){
    missing_names <- !(cn %in% colnames(a))
    if(any(missing_names)){
      new_names <- cn[missing_names]
      result <- data.frame(
        c(a, sapply(new_names, function(b){NA})),
        stringsAsFactors = FALSE)
      return(result[, cn])
    }else{
      return(a[, cn])
    }
  },
  cn = col_names_all
  )

  return(do.call(rbind, result_list))

}

# internal functions called by merge_columns
# ' Remove factors from an object
# '
# ' @description This function converts factors to characters to avoid errors with levels.
# ' @param z A data.frame
# ' @return Returns the input data.frame with all factors converted to character.
# ' @examples remove_factors(list(as.factor(c("a", "b"))))
remove_factors <- function(z){
  z[] <- lapply(z, function(x){
    if(is.factor(x)){as.character(x)}else{x}
  })
  return(z)
}
