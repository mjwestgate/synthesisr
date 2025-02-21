#' Clean a `tibble` or vector
#'
#' Cleans column and author names
#' @param data A `tibble` with bibliographic information.
#' @param x A vector of strings
#' @return Returns the input, but cleaner.
#' @example inst/examples/clean_.R
#' @name clean_
#' @export
clean_df <- function(data){
  colnames(data) <- clean_colnames(colnames(data))
  if(any(colnames(data) == "author")){
    data$author <- clean_authors(data$author)
  }
  data <- remove_factors(data)
  return(data)
}


# Standardize author delimiters
#' @rdname clean_
#' @export
clean_authors <- function(x){
  if(any(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))){
    x <- gsub("\\sAND\\s|\\s&\\s", " and ", x)
  }else{
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and ", x, perl = TRUE)
  }
  x <- gsub("\\s{2, }", " ", x)
  return(x)
}


# Clean common issues with column names
#' @rdname clean_
#' @export
clean_colnames <- function(
  x # colnames
){
  if(inherits(x, "data.frame")){
    x <- colnames(x)
  }
  x <- sub("^(X|Y|Z)\\.+", "", x) # remove leading X
  x <- sub("^[[:punct:]]*", "", x) # leading punctuation
  x <- sub("[[:punct:]]*$", "", x) # trailing punctuation
  x <- gsub("\\.+", "_", x) # replace 1 or more dots with underscore
  non_codes <- nchar(x) > 2 # for colnames with nchar > 2, convert to lower case
  x[non_codes] <- tolower(x[non_codes])
  x <- sub("authors", "author", x) # remove plural authors
  x <- make.unique(x, sep = "_")
  x <- gsub(" ", "_", x)
  return(x)
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
remove_factors <- function(z){
  z[] <- lapply(z, function(x){
    if(is.factor(x)){as.character(x)}else{x}
  })
  return(z)
}
