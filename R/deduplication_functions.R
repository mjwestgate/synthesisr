#' Remove punctuation from text
#' @description Removes common punctuation marks from a text
#' @param text the text from which to remove punctuation
#' @return the input text with punctuation removed
remove_punctuation <- function(text){
  punctuation <- c("\\!", "#", "$", "%", "&", "\\(", "\\)",
                   ",", "\\*", "\\.", "/", ":", ";",
                   "<", "=", ">", "\\?", "@", "\\[", "\\]",
                   "_", "`", "\\{", "\\|", "\\}", "~")
  for(i in 1:length(punctuation)){
    text <- gsub(punctuation[i], " ", text)
    if(i==length(punctuation)){text <- gsub("  ", " ", text)}
  }
  return(text)
}


#' Remove duplicate entries from a data frame
#' @description Given a data frame and a field to check for duplicates, flags and removes duplicate entries with three optional methods.
#' @param df the data frame to deduplicate
#' @param field the name or index of the column to check for duplicate values
#' @param method the manner of duplicate detection; quick removes exact text duplicates, similarity removes duplicates below a similarity threshold, and fuzzy uses fuzzdist matching
#' @param language the language to use if method is set to similarity
#' @param cutoff_distance the threshold below which articles are marked as duplicates by the similarity method
#' @return a deduplicated data frame
deduplicate <- function(df, field, method=c("quick", "similarity", "fuzzy"),
                               language="English", cutoff_distance=2){
  if(is.numeric(field)){
    target <- field
    field_name <- colnames(df)[field]
  } else if(!any(colnames(df)==field)){
    stop(print(paste(field), "does not match any column names in your data. Please use the exact column name and try again or specify a column number.", sep=" "))
  } else {
    target <- which(colnames(df)==field)
    field_name <- field
  }

  df[,target] <- synthesisr::remove_punctuation(df[,target])
  df[,target] <- stringr::str_trim(df[,target])

  if(any(df[,target]=="")){
    empty_fields <- which(df[,target]=="")
    for(i in 1:length(empty_fields)){
      df[empty_fields[i], target] <- paste("empty_field", i, sep="_")
    }
  }


  if(stringr::str_detect(tolower(field_name), "doi")){
    if(method=="similarity"){print("Note: it looks like your field contains DOIs so synthesisr switched the method to quick.")}
    method <- "quick"
  }

  if(method=="quick"){
    duplicates <- duplicated(df[,target])
    if(length(duplicates)>0){
      df <- df[-which(duplicates==TRUE),]
    }
  }

  if(method=="similarity"){
    mydfm <- synthesisr::create_dfm(elements=df[,target], type="tokens", language)
    mydist <- synthesisr::calculate_similarity(mydfm)
    df <- synthesisr::remove_similar(data=df, distance_data = mydist, id_column = 1, distance_column = 3, cutoff = cutoff_distance)
  }

  if(method=="fuzzy"){
    if (!requireNamespace("revtools", quietly = TRUE)){
      stop("revtools needed for this function to work. Please install it.",
           call. = FALSE)
    } else {

    df <- revtools::find_duplicates(data=df, match_variable = field, remove_punctuation = TRUE)
    }
  }

return(df)
}

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
      detections <- sapply(elements, stringr::str_detect, my_dictionary[i])
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


#' Detect multilingual texts
#' @description Detects texts that contain two languages in discrete blocks, such as a translation of a title
#' @param text the text to check for chimeras
#' @param overlap how large of a window to check for chimeras; an overlap of .5 will split the text into two chunks split at the middle, whereas higher levels of overlap will result in larger chunks to check
#' @return if a chimera is detected, the language codes of contained languages
chimera_detect <- function(text, overlap=.5){
  tokens <- strsplit(text, " ")[[1]]
  words <- length(tokens)
  first_half <- paste(tokens[1:(ceiling(words*overlap))], collapse=" ")
  second_half <- paste(tokens[floor((words-(words*overlap))):words], collapse=" ")
  lang1 <- synthesisr::language_detect(first_half)
  lang2 <- synthesisr::language_detect(second_half)
  if(lang1!=lang2){chimera <- paste(lang1, lang2)} else {
    chimera <- c()
  }
  return(chimera)
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

