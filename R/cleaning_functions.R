#' Cleans data.frames into synthesisr format
#'
#' @description Cleans column names and labels in data.frames into a format suitable for synthesisr applications.
#' @param data A data.frame with bibliographic information.
#' @return Returns the input data.frame cleaned and standardized.
#' @example inst/examples/clean_df.R
#' @rdname clean
clean_df <- function(data){
  colnames(data) <- clean_colnames(colnames(data))
  if(any(colnames(data) == "author")){
    data$author <- clean_authors(data$author)
  }
  return(data)
}


#' Standardize author delimiters
#'
#' @description This function standardizes delimiters between author names.
#' @param x Either a string or a vector of author names.
#' @return Returns the input vector with standardized delimiters.
#' @examples clean_authors(c("Darwin AND Wallace",  "Darwin & Wallace"))
#' @rdname clean
clean_authors <- function(x){
  if(any(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))){
    x <- gsub("\\sAND\\s|\\s&\\s", " and ", x)
  }else{
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and ", x, perl = TRUE)
  }
  x <- gsub("\\s{2, }", " ", x)
  return(x)
}


#' Clean common issues with column names
#'
#' @description Cleans column names from imports such as extra punctuation.
#' @param x A string or vector of column names.
#' @return Returns the input vector with common issues resolved.
#' @examples clean_colnames(c(".title...", "X..YEAR", "authors.."))
#' @rdname clean
clean_colnames <- function(
  x # colnames
){
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
