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

    for(i in 1:length(keywords)){
      detections <- sapply(elements, grep, my_dictionary[i])
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
#' Calculates similarity between documents
#' @description Computes the distance between documents in a document-feature matrix based on shared word associations.
#' @param dfm a document-feature matrix
#' @return a data frame with text distance scores for paired documents
calculate_similarity <- function(dfm){
  distance <- as.matrix(stats::dist(dfm))
  sim_mat <- distance
  sim_mat[lower.tri(sim_mat, diag=TRUE)] <- -999
  sim_vec <- as.vector(sim_mat)
  sim_vec <- sim_vec[which(sim_vec >= 0)]
  sim_mat <- as.data.frame(sim_mat)

  indices <- data.frame(ind = which(sim_mat >= 0, arr.ind=TRUE))
  indices$similarity <- sim_vec

  return(indices)
}

#' Removes similar documents based on text similarity
#' @description Removes documents from a data frame that are highly similar to other documents in the same data frame.
#' @param data the data frame containing all documents
#' @param distance_data a data frame with document identification and distance information
#' @param id_column the name or index of the column in the distance dataset that contains document IDs
#' @param distance_column the name or index of the column in the distance dataset that contains distance scores
#' @param cutoff the maximum distance at which documents should be considered duplicates
#' @return the documents data frame with duplicate documents removed
remove_similar <- function(data, distance_data, id_column, distance_column, cutoff){
  if(is.numeric(id_column) & id_column <= ncol(distance_data)){target <- id_column
  } else if(any(colnames(distance_data)==id_column)){
    target <- which(colnames(distance_data)==id_column)
  } else{stop("The provided id_column is not found in your distance data.")}

  if(is.numeric(distance_column) & distance_column <= ncol(distance_data)){distcol <- distance_column
  } else if(any(colnames(distance_data)==id_column)){
    distcol <- which(colnames(distance_data)==distance_column)
  } else{stop("The provided distance_column is not found in your distance data.")}

  too_similar <- which(distance_data[,distcol] < cutoff)
  removals <- unique(distance_data[too_similar, target])

  data <- data[-removals,]
  return(data)
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


#' Detect the language of a text
#' @description Uses common stopwords to assign probable language(s) to a text
#' @param text the text to which to assign a language
#' @return a character vector of probable languages
language_detect <- function(text){
  tokens <- strsplit(synthesisr::remove_punctuation(text), " ")[[1]]
  for(i in 1:length(stopwords::stopwords_getlanguages("stopwords-iso"))){
    lang <- stopwords::stopwords_getlanguages("stopwords-iso")[i]
    stops <- stopwords::stopwords(lang, source="stopwords-iso")
    counter <- 0
    for(j in 1:length(tokens)){
    detected <- any(stops==tokens[j])
    if(detected==TRUE){counter <- counter+1}
    }

    if(i==1){
      all_languages <- cbind(lang, counter)
    }
    if(i>1){
      current_lang <- cbind(lang, counter)
      all_languages <- rbind(all_languages, current_lang)
    }

  }

  probable_language <- as.character(all_languages[which(all_languages[,2]==max(all_languages[,2])),1])
  if(length(probable_language)>2){stop("Language cannot be determined.")}
  return(probable_language)
}

