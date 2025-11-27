#' Clean a `tibble` or vector
#'
#' Cleans column and author names
#' @param x A `tibble` with bibliographic information.
#' @return Returns the input, but cleaner.
#' @example inst/examples/clean_.R
#' @name clean_
#' @export
clean_df <- function(x){
  x |>
    clean_colnames() |>
    clean_authors() |>
    convert_factors_to_strings()
}


# Standardize author delimiters
#' @rdname clean_
#' @export
clean_authors <- function(x){
  if(inherits(x, "data.frame")){
    if(any(colnames(x) == "author")){
      if(is.character(x$author)){ # exception added to ensure list-cols are ignored
        x$author <- clean_authors_regex(x$author)
      }
    }
  }else if(is.character(x)){
    x <- clean_authors_regex(x)
  }
  x
}

#' Internal function to enact clean_authors
#' @noRd
#' @keywords Internal
clean_authors_regex <- function(x){
  if(any(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))){
    x <- stringr::str_replace_all(x, "\\sAND\\s|\\s&\\s", " and ")
  }else{
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and ", x, perl = TRUE)
  }
  stringr::str_replace_all(x, "\\s+", " ")
}

# Clean common issues with column names
#' @rdname clean_
#' @export
clean_colnames <- function(
  x # a tibble
){
  # basic cleaning pipe
  newcolnames <- colnames(x) |>
    stringr::str_replace("^(X|Y|Z)\\.+", "") |> # remove leading X
    stringr::str_replace("^[[:punct:]]*", "") |>  # leading punctuation
    stringr::str_replace("[[:punct:]]*$", "") |> # trailing punctuation
    stringr::str_replace("authors", "author") |> # remove plural authors
    stringr::str_replace_all("\\.+", "_") |> # replace 1 or more dots with underscore
    stringr::str_replace_all(" ", "_")

  # break pipe to conditionally change strings based on length
  non_codes <- nchar(newcolnames) > 2 # for colnames with nchar > 2, convert to lower case
  newcolnames[non_codes] <- tolower(newcolnames[non_codes])
  colnames(x) <- make.unique(newcolnames, sep = "_")
  x
}

#' Remove factors from an object
#'
#' Internal functions called by `clean_df()`:
#' @description This function converts factors to characters to avoid errors with
#' levels.
#' @param z A data.frame
#' @return Returns the input data.frame with all factors converted to character.
#' @noRd
#' @keywords Internal
convert_factors_to_strings <- function(z){
  z[] <- lapply(z, function(x){
    if(is.factor(x)){
      as.character(x)
    }else{
      x
    }
  })
  return(z)
}
