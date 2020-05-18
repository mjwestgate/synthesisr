# Cleans data.frames into synthesisr format
#' @rdname clean_
clean_df <- function(data){
  colnames(data) <- clean_colnames(colnames(data))
  if(any(colnames(data) == "author")){
    data$author <- clean_authors(data$author)
  }
  return(data)
}


# Standardize author delimiters
#' @rdname clean_
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
