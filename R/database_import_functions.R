#' Import .bib and .ris files
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_revtools <- function(df){
  df <- as.data.frame(cbind(id=df$label, title=df$title, abstract=df$abstract, keywords=df$keywords, type=df$type, authors=df$author,
                            affiliation=df$institution, source=df$journal, year=df$year, volume=df$volume, issue=df$issue,
                            startpage=df$pages, doi=df$doi, language=df$language))
  df$methods <- rep("", length(df$id))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$startpage <- as.character(df$startpage)
  df$endpage <- as.character(df$startpage)
  temp <- strsplit(as.character(df$startpage), "-")
  if (length(temp) > 0) {
    for (j in 1:length(temp)) {
      df$startpage[j] <- temp[[j]][1]
      if (length(temp[[j]]) > 1) {
        df$endpage[j] <- temp[[j]][2]
      }
    }
  }
  return(df)
}


#' Import .csv exported from Zotero
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_Zotero <- function(df){
  df <- as.data.frame(cbind(id = df$ISBN,
                            title = df$Title, abstract = df$Abstract.Note,
                            authors = df$Author, source = df$Publication.Title,
                            year = df$Publication.Year, volume = df$Volume,
                            issue = df$Issue, startpage = df$Pages,
                            doi = df$DOI, keywords = df$Manual.Tags,
                            type = df$Type, language=df$Language))
  df$methods <- rep("", nrow(df))
  df$affiliation <- rep("", nrow(df))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$startpage <- as.character(df$startpage)
  df$endpage <- df$startpage
  temp <- strsplit(as.character(df$startpage), "-")
  if (length(temp) > 0) {
    for (j in 1:length(temp)) {
      df$startpage[j] <- temp[[j]][1]
      if (length(temp[[j]]) > 1) {
        df$endpage[j] <- temp[[j]][2]
      }
    }
  }
  df$database <- rep("Zotero", nrow(df))
  return(df)
}

#' Import .csv from Scopus
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_Scopus <- function(df){
  df <- as.data.frame(cbind(id = df$EID, title = df$Title,
                            abstract = df$Abstract, keywords = df$Author.Keywords,
                            type = df$Document.Type, authors = df$Authors,
                            affiliation = df$Affiliations, source = df$Source.title,
                            year = df$Year, volume = df$Volume, issue = df$Issue,
                            startpage = df$Page.start, endpage = df$Page.end,
                            doi = df$DOI))
  df$methods <- rep("", length(df$id))
  df$language <- rep("", length(df$id))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$database <- rep("Scopus", nrow(df))

  return(df)
}

#' Import .txt from Zoological Record
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_ZooRec <- function(df){
  df <- as.data.frame(cbind(id = df$AN, title = df$TI,
                            abstract = df$AB, keywords = df$DE, type = df$DT,
                            authors = df$AU, affiliation = df$C1, source = df$SO,
                            year = df$PY, volume = df$VL, issue = df$IS,
                            startpage = df$PS, doi = df$DI, language = df$LA))
  df$startpage <- as.character(df$startpage)
  df$endpage <- rep("", nrow(df))
  temp <- strsplit(as.character(df$startpage), "-")
  if (length(temp) > 0) {
    for (j in 1:length(temp)) {
      df$startpage[j] <- temp[[j]][1]
      if (length(temp[[j]]) > 1) {
        df$endpage[j] <- temp[[j]][2]
      }
    }
  }
  df$methods <- rep("", length(df$id))
  df$text <- paste(df$title, df$abstract, sep = " ")

  df$database <- rep("ZooRec", nrow(df))

  return(df)
}

#' Import .txt from BIOSIS and several other Web of Science databases
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_BIOSIS <- function(df){
  df <- as.data.frame(cbind(id = df$UT, title = df$TI,
                            abstract = df$AB, methods = df$MQ, keywords = df$MI,
                            type = df$DT, authors = df$AU, affiliation = df$C1,
                            source = df$SO, year = df$PY, volume = df$VL,
                            issue = df$IS, startpage = df$BP, endpage = df$EP,
                            doi = df$DI, language = df$LA))
  df$text <- paste(df$title, df$abstract, sep = " ")

  df$database <- rep("BIOSIS", nrow(df))

  return(df)
}

#' Import files from Core Collections
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_Core <- function(df){
  df <- as.data.frame(cbind(id = df$UT, title = df$TI,
                            abstract = df$AB, methods = df$MQ, keywords = df$DE,
                            type = df$DT, authors = df$AU, affiliation = df$C1,
                            source = df$SO, year = df$PY, volume = df$VL,
                            issue = df$IS, startpage = df$BP, endpage = df$EP,
                            doi = df$DI, language = df$LA))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$database <- rep("CoreCollections", nrow(df))
  return(df)
}


#' Import from miscellaneous Web of Science databases
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_OtherWoS <- function(df){
  df <- as.data.frame(cbind(id = df$UT, title = df$TI,
                            abstract = df$AB, keywords = df$DE,
                            type = df$DT, authors = df$AU,
                            source = df$SO, year = df$PY, volume = df$VL,
                            issue = df$IS, startpage = df$BP, endpage = df$EP,
                            doi = df$DI, language = df$LA))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$methods <- rep("", nrow(df))
  df$affiliation <- rep("", nrow(df))
  df$database <- rep("Web_of_Science_generic", nrow(df))
  return(df)
}

#' Import Web of Science MEDLINE
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_MEDLINE <- function(df){
  df <- as.data.frame(cbind(id = df$AN, title = df$TI,
                            abstract = df$AB, keywords = df$ID, type = df$DT,
                            authors = df$AU, affiliation = df$C1, source = df$SO,
                            year = df$Y, volume = df$VL, issue = df$IS, startpage = df$PS,
                            doi = df$DI, language = df$LA))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$methods <- rep("", length(df$id))
  temp <- strsplit(as.character(df$startpage), "-")
  if (length(temp) > 0) {
    for (j in 1:length(temp)) {
      df$startpage[j] <- temp[[j]][1]
      if (length(temp[[j]]) > 1) {
        df$endpage[j] <- temp[[j]][2]
      }
    }
  }

  df$database <- rep("MEDLINE", nrow(df))
  return(df)
}

#' Import from Engineering Village
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_EngVill <- function(df){
  df <- as.data.frame(cbind(id = df$Accession.number, title = df$Title,
                            abstract = df$Abstract, keywords = df$Controlled.Subject.terms,
                            type = df$Document.type, authors = df$Author, affiliation = df$Author.affiliation,
                            source = df$Source, year = df$Publication.year, volume = df$Volume,
                            issue = df$Issue, startpage = df$Pages,
                            doi = df$DOI, language = df$Language))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$methods <- rep("", nrow(df))
  df$startpage <- as.character(df$startpage)
  temp <- strsplit(as.character(df$startpage), "-")
  if (length(temp) > 0) {
    for (j in 1:length(temp)) {
      df$startpage[j] <- temp[[j]][1]
      if (length(temp[[j]]) > 1) {
        df$endpage[j] <- temp[[j]][2]
      }
    }
  }

  df$database <- rep("EngineeringVillage", nrow(df))
  return(df)
}

#' Import from generic EBSCO results across several databases
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_EBSCO <- function(df){
  df <- as.data.frame(cbind(id = df$Accession.Number,
                            title = df$Article.Title, abstract = df$Abstract,
                            authors = df$Author, source = df$Journal.Title,
                            year = df$Publication.Date, volume = df$Volume,
                            issue = df$Issue, startpage = df$First.Page,
                            endpage = df$Page.Count, doi = df$DOI, keywords = df$Keywords,
                            type = df$Doctype))
  df$methods <- rep("", nrow(df))
  df$affiliation <- rep("", nrow(df))
  df$language <- rep("", nrow(df))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$database <- rep("EBSCO_generic", nrow(df))
  return(df)
}

#' Import from the Networked Digital Library of Theses and Dissertations
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_NDLTD <- function(df){
  df <- as.data.frame(cbind(title=df$title, authors=df$author,
                            year=df$date, abstract=df$abstract))
  df$id <- rep("", nrow(df))
  df$source <- rep("", nrow(df))
  df$volume <- rep("", nrow(df))
  df$issue <- rep("", nrow(df))
  df$startpage <- rep("", nrow(df))
  df$endpage <- rep("", nrow(df))
  df$doi <- rep("", nrow(df))
  df$keywords <- rep("", nrow(df))
  df$type <- rep("", nrow(df))
  df$methods <- rep("", nrow(df))
  df$affiliation <- rep("", nrow(df))
  df$language <- rep("", nrow(df))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$database <- rep("NDLTD", nrow(df))
  return(df)
}

#' Import from Open Access Theses and Dissertations
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_OATD <- function(df){
  df <- as.data.frame(cbind(title=df$title, authors=df$author,
                            abstract=df$abstract))
  df$id <- rep("", nrow(df))
  df$source <- rep("", nrow(df))
  df$volume <- rep("", nrow(df))
  df$issue <- rep("", nrow(df))
  df$startpage <- rep("", nrow(df))
  df$endpage <- rep("", nrow(df))
  df$doi <- rep("", nrow(df))
  df$keywords <- rep("", nrow(df))
  df$type <- rep("", nrow(df))
  df$methods <- rep("", nrow(df))
  df$affiliation <- rep("", nrow(df))
  df$language <- rep("", nrow(df))
  df$year <- rep("", nrow(df))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$database <- rep("OATD", nrow(df))
  return(df)
}

#' Import from OpenThesis.org
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_OpenThesis <- function(df){
  df <- as.data.frame(cbind(title=df$title, authors=df$author,
                            year=df$date))
  df$id <- rep("", nrow(df))
  df$abstract <- rep("", nrow(df))
  df$source <- rep("", nrow(df))
  df$volume <- rep("", nrow(df))
  df$issue <- rep("", nrow(df))
  df$startpage <- rep("", nrow(df))
  df$endpage <- rep("", nrow(df))
  df$doi <- rep("", nrow(df))
  df$keywords <- rep("", nrow(df))
  df$type <- rep("", nrow(df))
  df$methods <- rep("", nrow(df))
  df$affiliation <- rep("", nrow(df))
  df$language <- rep("", nrow(df))
  df$text <- paste(df$title, df$abstract, sep = " ")
  df$database <- rep("OpenThesis", nrow(df))
  return(df)
}

#' Import from generic ProQuest .xls results
#' @description Imports files to synthesis from a data frame. Intended primarily as a function to be called by import_results()
#' @param df the data frame to import
#' @return the data frame with synthesisr formatted columns
import_ProQuest <- function(df){
  df <- as.data.frame(cbind(id=df$StoreId, title=df$Title, abstract=df$Abstract, keywords=df$subjectTerms, type=df$documentType,
                            authors=df$Authors, source=df$pubtitle, year=df$year, volume=df$volume, issue=df$issue,
                            startpage=df$pages, doi=df$digitalObjectIdentifier, language=df$language))
  df$affiliation <- rep("", nrow(df))
  df$methods <- rep("", nrow(df))
  df$endpage <- rep("", nrow(df))

  if(any(colnames(df)=="startpage")){
    df$startpage <- as.character(df$startpage)
    temp <- strsplit(as.character(df$startpage), "-")
    if (length(temp) > 0) {
      for (j in 1:length(temp)) {
        if(length(temp[[j]])>0){
          df$startpage[j] <- temp[[j]][1]
        }
        if (length(temp[[j]]) > 1) {
          df$endpage[j] <- temp[[j]][2]
        }
      }
    }
  } else{
    df$startpage <- rep("", nrow(df))
    df$endpage <- rep("", nrow(df))
  }


  df$text <- paste(df$title, df$abstract, sep=" ")
  df$database <- rep("ProQuest_generic", nrow(df))
  return(df)
}



