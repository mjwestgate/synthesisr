#' Create a document-feature matrix
#' @description Given a character vector of document information, creates a document-feature matrix.
#' @param elements a character vector of document information (e.g. document titles or abstracts)
#' @param type whether the dfm should be created based on document tokens or a restricted list of keywords
#' @param language if type="tokens", the language to use for removing stopwords
#' @param keywords if type="keywords", a character vector of keywords to use as document features
#' @return a matrix with documents as rows and terms as columns
create_dfm <- function(elements, type=c("tokens", "keywords"), language="English", keywords=NULL){
  if(type=="tokens"){
    corp <- tm::VCorpus(tm::VectorSource(elements))
    dfm <- tm::DocumentTermMatrix(corp)
  }
  start <- Sys.time()
  if(type=="keywords"){
    elements <- tolower(elements)
    my_dictionary <- keywords
    dfm <- matrix(data=NA, nrow=length(elements),
                  ncol=length(my_dictionary),
                  byrow = TRUE,
                  dimnames = list(1:length(elements), my_dictionary))

# need to turn this into lapply or something much more efficient than it is
    for(i in 1:length(keywords)){
      detections <- sapply(elements, grep, paste("\\b", my_dictionary[i], sep=""))
      names(detections) <- NULL
      detections <- as.numeric(detections)

      dfm[,i] <- detections
    }
  }
  end <- Sys.time()
  runtime <- end-start
  dfm <- as.matrix(dfm)
  return(dfm)
}

#' Get short language codes
#' @description Returns the two-letter language code for specified language
#' @param language a character vector containing the name of a language
#' @return a character vector containing a two-letter language code
language_code <- function(language){
  if(nchar(language==2)){la_code <- language}
  if(nchar(language)>2){

    reference <- synthesisr::possible_langs
    la_code <- as.character(reference$Short[which(reference$Language==language)])
  }
  return(la_code)
}

#' Retrieve stopwords for a given language
#' @description Returns a character vector of stopwords to use for a specified language
#' @param language a character vector containing the name of the language for which to retrieve stopwords
#' @return a character vector of stopwords
get_stopwords <- function(language){
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
#' @description Removes stopwords from text in user-specified language
#' @param text the text from which to remove stopwords
#' @param language the language of the text
#' @return the text with stopwords removed
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
#' @description Removes stopwords and extracts tokens from text
#' @param text the text from which to remove stopwords
#' @param language the language of the text
#' @return a character vector of tokens from the text
get_tokens <- function(text, language){
  text <- tolower(text)
  text <- synthesisr::remove_stopwords(text=text, language=language)
  text <- tm::remove_punctuation(text)
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

