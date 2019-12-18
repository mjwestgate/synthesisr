#' Checks directory files to see if they can be imported
#' @description Checks files staged for import to ensure they are a matching filetype and removes unsupported filetypes or directories from the list.
#' @param import_files a character vector with paths to files for import
#' @return a character vector with paths to files for import with unsupported filetypes removed
check_filetypes <- function(import_files){
  allowed_filetypes <- c("csv", "txt", "xml", "xls", "bib", "nbib", "ris")
  for(i in 1:length(import_files)){
  if(i==1){removals <- c()}
  filetype <- synthesisr::detect_filetype(import_files[i])
  if(is.na(filetype)){filetype <- "nope"}
  if(!any(allowed_filetypes==filetype)){
    print(paste("File format is not recognized. Skipping", import_files[i]))
    removals <- append(removals, i)
  }
  if(i==length(import_files)){
    if(length(removals) > 0){
      import_files <- import_files[-removals]
    }
  }
  }

  return(import_files)
}


#' Reads in files to import
#' @description Given a path to a file, determines file type and reads it.
#' @param file a path to a file of a supported type
#' @return a dataframe with the contents of the file
read_files <- function(file){
  filetype <- synthesisr::detect_filetype(file)
  readable <- synthesisr::check_filetypes(file)
  if(length(readable)==0){stop("File type not recognized.")}
  if(filetype=="csv"){
    df <- utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  }
  if(filetype=="xml"){
    df <- as.character(xml2::read_xml(file))
  }
  if(filetype=="txt") {
    df <- utils::read.delim(file, sep = "\t", header = TRUE,
                     stringsAsFactors = FALSE, fill = TRUE, row.names = NULL)
    if(colnames(df)[1]=="row.names"){
      colnames(df) <- append(colnames(df[2:length(df)]), "X")
    }

  }
  if(filetype=="xls") {
    df <- xlsx::read.xlsx(file, 1)
    df[] <- lapply(df, function(x) if(is.factor(x)) as.character(x) else x)
  }

  if(any(c("bib", "nbib", "ris")==filetype)){
    if (!requireNamespace("revtools", quietly = TRUE)){
      stop("revtools needed to import .bib and .ris files. Please install it.",
           call. = FALSE)
    } else {

    df <- revtools::read_bibliography(file)
    }
    }

  return(df)

}


#' Convert dataframes to standard format
#' @description Given a data frame, checks to see that it contains standard synthesisr fields and fills empty fields.
#' @param df a data frame
#' @return a data frame with standardized columns for first 17 fields, plus other fields specific to the dataset
standardize_df <- function(df){
  checks <- c("id", "text", "title", "abstract", "keywords", "methods", "type", "authors", "affiliation", "journal",
              "year", "volume", "issue", "pages", "doi", "language", "database")
  for(c in 1:length(checks)){
    if(checks[c] %in% colnames(df)==FALSE){
      if(c==1){df$id <- 1:nrow(df)}
      if(c==2){df$text <- rep("", nrow(df))}
      if(c==3){df$title <- rep("", nrow(df))}
      if(c==4){df$abstract <- rep("", nrow(df))}
      if(c==5){df$keywords <- rep("", nrow(df))}
      if(c==6){df$methods <- rep("", nrow(df))}
      if(c==7){df$type <- rep("", nrow(df))}
      if(c==8){df$authors <- rep("", nrow(df))}
      if(c==9){df$affiliation <- rep("", nrow(df))}
      if(c==10){df$source <- rep("", nrow(df))}
      if(c==11){df$year <- rep("", nrow(df))}
      if(c==12){df$volume <- rep("", nrow(df))}
      if(c==13){df$issue <- rep("", nrow(df))}
      if(c==14){df$pages <- rep("", nrow(df))}
      if(c==15){df$doi <- rep("", nrow(df))}
      if(c==16){df$language <- rep("", nrow(df))}
      if(c==17){df$database <- rep("", nrow(df))}

    }
  }
  not_checked <- df[,-which(colnames(df) %in% checks)]

    df[] <- lapply(df, as.character)
    df <- as.data.frame(cbind(id = df$id,
                              text = df$text,
                              title = df$title,
                              abstract = df$abstract,
                              keywords = df$keywords,
                              methods = df$methods,
                              type = df$type,
                              authors = df$authors,
                              affiliation = df$affiliation,
                              journal = df$journal,
                              year = df$year,
                              volume = df$volume,
                              issue = df$issue,
                              pages = df$pages,
                              doi = df$doi,
                              language = df$language,
                              database = df$database))
    df[] <- lapply(df, as.character)
    df <- cbind(df, not_checked)

    return(df)
}


#' Detects file types
#' @description Given a file, determines the file extension
#' @param file a path to a file
#' @return a character vector with the likely file type based on the file extension
detect_filetype <- function(filename){
  file_extension_lookup <- regexpr(".[[:alnum:]]{2,}$", filename)
  file_type <- substr(filename, file_extension_lookup+1, nchar(filename))
  return(file_type)
}



#' Import results of a systematic review
#' @description Given a file or directory, imports and assembles search results
#' @param directory a path to a directory containing search results to import
#' @param filename a path to a filename containing search results to import
#' @param verbose if TRUE, prints status updates
#' @param retain_all_output if TRUE, stores each search result as a list entry with original fields; if FALSE, merges all search output to a common 17 bibliographic fields
#' @return a data frame of assembled search results
import_results <- function(directory=NULL, filename=NULL, verbose = TRUE, retain_all_output=FALSE){

  if(!is.null(directory)){import_files <- paste(directory, list.files(path = directory), sep = "")} else if(!is.null(filename)){import_files <- filename}
  if(is.null(directory) & is.null(filename)){stop("No input given. Either directory or filename needs to be provided.")}

  if(retain_all_output==TRUE){
    search_list <- list()
  }

  import_files <- synthesisr::check_filetypes(import_files)

  for(i in 1:length(import_files)){
    filename <- import_files[i]

    df <- synthesisr::read_files(filename)

    if(verbose==TRUE){print(paste("Importing file", import_files[i]))}

    if(any(c("bib", "nbib", "ris")==synthesisr::detect_filetype(import_files[i]))){
      df <- df
    } else {
    df <- synthesisr::match_columns(df)
    }

    df <- synthesisr::standardize_df(df)

    if (i == 1) {
        search_hits <- df[,1:17]
      }
      if (i > 1) {
        search_hits <- rbind(search_hits, df[,1:17])
      }
    if(retain_all_output==TRUE){
      search_list[[i]] <- df
    }
    }

  if(retain_all_output==TRUE){
    return(search_list)
  } else{return(search_hits)}
}

#' Match imported data to reference codes
#' @description Takes an imported dataframe, rearranges it to match lookup codes, and removes redundant columns
#' @param df a data frame that contains bibliographic information
#' @return a data frame rearranged and coded to match standard bibliographic fields, with other fields appended
match_columns <- function(df){
  # Thanks for Martin's code in revtools for the idea to use lookups! ...and some of the tag lookups

  # figure out which columns match known tags
  hits <- as.numeric(match(synthesisr::code_lookup$code, colnames(df)))

  # rearrange data in standard order
  newdat <- df[, hits[!is.na(hits)]]

  # retain columns even if they did not match lookup
  newcolnames <- synthesisr::code_lookup$field[match(colnames(newdat), synthesisr::code_lookup$code)]
  newcolnames[which(is.na(newcolnames) | newcolnames=="")] <- colnames(newdat)[which(is.na(newcolnames) | newcolnames=="")]
  colnames(newdat) <- newcolnames

  # drop duplicate columns that matched more than one tag
  # often, duplicates are multiple author fields, pages and startpage/endpage, etc
  newdat <- newdat[,-which(duplicated(colnames(newdat)))]

  return(newdat)
}


