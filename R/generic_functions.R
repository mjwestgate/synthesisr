#' Create a document-feature matrix
#'
#' @description Given a character vector of document information, creates a document-feature matrix.
#' @param elements A character vector of document information (e.g. document titles or abstracts)
#' @param features A character vector of terms to use as document features (e.g. keywords)
#' @param closure Any restrictions on if terms should be closed when detecting matches.
#' @param ignore_case Logical: Should case be ignored when detecting features within documents?
#' @return Returns a matrix with documents as rows and terms as columns.
#' @note When matching with closure, left requires terms to start with a keyword (e.g "burn" matches "burning"), right requires terms to end with a keyword (e.g. "burn" matches "postburn" but not "postburning"), full requires exact matches (e.g. "burn" only matches "burn"), and none allows keywords to be embedded within terms (e.g. "burn" matches "postburning").
#' @example inst/examples/create_dfm.R
create_dfm <- function(elements, features, closure=c("left", "right", "full", "none"), ignore_case=TRUE){
  if(ignore_case==TRUE){
    elements <- tolower(elements)
    features <- tolower(features)
  }

  my_dictionary <- switch (closure,
                           "left" = {my_dictionary <- paste("\\b", features, sep="")},
                           "right" = {my_dictionary <- paste(features, "\\b", sep="")},
                           "full" = {my_dictionary <- paste("\\b", features, "\\b", sep="")},
                           "none" = {my_dictionary <- features}
  )

  dfm <- sapply(my_dictionary, grepl, x=elements)
  if(closure!="none"){
    colnames(dfm) <- gsub("\\\\b", "", colnames(dfm))
  }
  return(dfm)
}

#' Get short language codes
#'
#' @description This is a lookup function that returns the two-letter language code for specified language.
#' @param language A character vector containing the name of a language.
#' @return Returns a character vector containing a two-letter language code.
#' @examples language_code("French")
language_code <- function(language){
  if(nchar(language==2)){la_code <- language}
  if(nchar(language)>2){

    reference <- synthesisr::possible_langs
    la_code <- as.character(reference$Short[which(reference$Language==language)])
  }
  return(la_code)
}

#' Retrieve stopwords for a given language
#'
#' @description This function retrieves stopwords to use for a specified language.
#' @param language A character vector containing the name of the language for which to retrieve stopwords.
#' @return Returns a character vector of stopwords.
#' @examples get_stopwords("English")
get_stopwords <- function(language){
  if(!requireNamespace("stopwords")){
    stop("Package 'stopwords' needed for this function to work. Please install it.")
  }
  if(!any(synthesisr::possible_langs$Language==language)){stop("The language you specified is not supported.")} else {
    la_code <- language_code(language)
  }

  if(length(la_code) > 0){

    if(la_code=="en"){stopwords <- stopwords::stopwords("en", source="smart")
    } else if(any(stopwords::stopwords_getlanguages("snowball")==la_code)){
      stopwords <- stopwords::stopwords(la_code, source="snowball")
    } else if (any(stopwords::stopwords_getlanguages("stopwords-iso")==la_code)){
      stopwords <- stopwords::stopwords(la_code, source="stopwords-iso")
    } else {stop("The language you specified is not supported.")}
  }
  return(stopwords)
}

#' Remove stopwords from text
#'
#' @description Removes stopwords from text in whichever language is specified.
#' @param text A character vector containing text from which to remove stopwords.
#' @param language A string indicating the language of the text.
#' @return Returns the input text with stopwords removed.
#' @examples remove_stopwords("On the Origin of Species", language="English")
remove_stopwords <- function(text, language){

  stopwords <- synthesisr::get_stopwords(language)
  stopwords <- paste("\\b", stopwords, "\\b", sep="")

  # another for-loop that needs to be more efficient
  for(i in 1:length(stopwords)){
    text <- gsub(stopwords[i], " ", text)
  }
  text <- gsub("  ", " ", text)
  text <- gsub("  ", " ", text)

  return(text)
}

#' Retrieves tokens from a text
#'
#' @description This function removes stopwords and extracts tokens from text.
#' @param text A character vector containing text from which to extract tokens.
#' @param language A string indicating the language of the text.
#' @return Returns a character vector of tokens from the text.
#' @examples get_tokens("On the Origin of Species", language = "English")
get_tokens <- function(text, language){
  text <- tolower(text)
  text <- synthesisr::remove_stopwords(text=text, language=language)
  text <- synthesisr::remove_punctuation(text)
  tokens <- strsplit(text, " ")[[1]]
  if(any(is.na(tokens))){
    tokens <- tokens[-is.na(tokens)]
  }
  if(any(tokens=="")){
    tokens <- tokens[-which(tokens=="")]
  }
  if(any(tokens=="")){
    tokens <- tokens[-which(tokens==" ")]
  }
  if(any(tokens=="'")){
    tokens <- tokens[-which(tokens=="'")]
  }
  return(tokens)
}

#' Remove punctuation from text
#'
#' @description Removes common punctuation marks from a text.
#' @param text A character vector from which to remove punctuation.
#' @param remove_hyphens Logical: Should hyphens be considered punctuation and removed?
#' @return Returns the input text with punctuation removed.
#' @examples remove_punctuation("#s<<<//<y>!&^n$$t/>h%e&s$is#!++r!//")
remove_punctuation <- function(text, remove_hyphens=FALSE){
  if(remove_hyphens==TRUE){output <- gsub("[[:punct:]]", "\\1", text)}else{
    output <- gsub("([-])|[[:punct:]]", "\\1", text)
  }

  if(any(grepl(" -", output))){
    while(any(grepl(" -", output))){
      output <- gsub(" -", "-", output)
    }
  }

  if(any(grepl("  ", output))){
    while(any(grepl("  ", output))){
      output <- gsub("  ", " ", output)
    }
  }

  return(output)

}



#' Remove numbers from text
#'
#' @description Removes numbers from a text.
#' @param text A character vector from which to remove numbers.
#' @return Returns the input text with numbers removed.
#' @examples remove_numbers("11s0y6nt4he35si6sr")
remove_numbers <- function(text){
  output <- gsub("[[:digit:]]", "", text)

  if(any(grepl("  ", output))){
    while(any(grepl("  ", output))){
      output <- gsub("  ", " ", output)
    }
  }

  return(output)

}
