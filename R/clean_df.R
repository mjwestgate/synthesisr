#' Cleans data.frames into synthesisr format
#'
#' @description Cleans column names and labels in data.frames into a format suitable for synthesisr applications.
#' @param data A data.frame with bibliographic information.
#' @return Returns the input data.frame cleaned and standardized.
#' @seealso \code{\link{clean_authors}} and \code{\link{clean_names}} for underlying functions.
#' @example inst/examples/clean_df.R
clean_df <- function(data){
  colnames(data) <- synthesisr::clean_names(colnames(data))
  if(colnames(data)[1] != "label"){
    if(
      length(unique(data[, 1])) < nrow(data) |
      any(c("author", "title", "year", "journal") == colnames(data)[1])
    ){
      data <- data.frame(
        label = synthesisr::create_index("ref", nrow(data)),
        data,
        stringsAsFactors = FALSE
      )
    }
  }
  if(any(colnames(data) == "author")){
    data$author <- synthesisr::clean_authors(data$author)
  }
  return(data)
}


#' Standardize author delimiters
#'
#' @description This function standardizes delimiters between author names.
#' @param x Either a string or a vector of author names.
#' @return Returns the input vector with standardized delimiters.
#' @examples clean_authors(c("Pullin AND Knight",  "Pullin & Knight"))
clean_authors <- function(x){
  if(any(grepl("\\sand\\s|\\sAND\\s|\\s&\\s", x))){
    x <- gsub("\\sAND\\s|\\s&\\s", " and ", x)
  }else{
    x <- gsub(",(?=\\s[[:alpha:]]{2,})", " and ", x, perl = TRUE)
  }
  x <- gsub("\\s", " ", x)
  if(any(grepl("  ", x))){
    while(grepl("  ", x)){
      x <- gsub("  ", " ", x)
    }
  }
  return(x)
}

#' Create a string to sort in correct order
#'
#' @description Creates a string of named length in format "string_number" to index in correct order.
#' @param string A string to precede numbers in the index.
#' @param n Numeric: the number of unique index values to create.
#' @param sep A string to serve as the separator between the input string and number.
#' @return A character vector in "string_number" format.
#' @examples create_index(n=2)
create_index <- function(string, n, sep = "_"){
  if(missing(string)){
    string <- "V"
  }
  if(missing(n)){
    stop("n is missing from create_index with no default")
  }
  if(n < 1){
    stop("n must be > 0 for create_index to function")
  }
  if(length(n) > 1){
    n <- length(n)
  }
  size <- log10(n) + 1
  vector <- seq_len(n)
  return(
    paste(
      string,
      formatC(vector, width  = size, format = "d", flag = 0),
      sep = sep
    )
  )
}


#' Clean common issues with column names
#'
#' @description Cleans column names from imports such as extra punctuation.
#' @param x A string or vector of column names.
#' @return Returns the input vector with common issues resolved.
#' @examples clean_names(c(".title...", "X..YEAR", "authors.."))
clean_names <- function(
  x # colnames
){
  x <- sub("^(X|Y|Z)\\.+", "", x) # remove leading X
  x <- sub("^[[:punct:]]*", "", x) # leading punctuation
  x <- sub("[[:punct:]]*$", "", x) # trailing punctuation
  x <- gsub("\\.+", "_", x) # replace 1 or more dots with underscore
  x <- tolower(x)
  x <- sub("authors", "author", x) # remove plural authors
  x <- make.unique(x, sep = "_")
  return(x)
}
