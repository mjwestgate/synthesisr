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
    df <- utils::read.table(file, sep = "\t", header = TRUE,
                     comment.char = "#", na.strings = ".", stringsAsFactors = FALSE,
                     quote = "", fill = TRUE, row.names = NULL)
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
#' @return a data frame with standardized columns
standardize_df <- function(df){
  checks <- c("id", "text", "title", "abstract", "keywords", "methods", "type", "authors", "affiliation", "source",
              "year", "volume", "issue", "startpage", "endpage", "doi", "language", "database")
  for(c in 1:length(checks)){
    if(stringr::str_detect(paste(colnames(df), collapse=" "), checks[c])==FALSE){
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
      if(c==14){df$startpage <- rep("", nrow(df))}
      if(c==15){df$endpage <- rep("", nrow(df))}
      if(c==16){df$doi <- rep("", nrow(df))}
      if(c==17){df$language <- rep("", nrow(df))}
      if(c==18){df$database <- rep("", nrow(df))}

    }
  }

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
                              source = df$source,
                              year = df$year,
                              volume = df$volume,
                              issue = df$issue,
                              startpage = df$startpage,
                              endpage = df$endpage,
                              doi = df$doi,
                              language = df$language,
                              database = df$database))
    df[] <- lapply(df, as.character)

    return(df)
}


#' Detects file types
#' @description Given a file, determines the file extension
#' @param file a path to a file
#' @return a character vector with the likely file type based on the file extension
detect_filetype <- function(file){
  filetype <- strsplit(substring(file, (nchar(file)-4), nchar(file)), "\\.")[[1]][2]
  return(filetype)
}


#' Detects from which database a search originated
#' @description Determines the database from which a search results file originated, if supported and exported per synthesisr instructions.
#' @param df a data frame of search results from a single source
#' @return a character vector specifying the platform and/or database of origin, if known
detect_database <- function(df){
  database <- NULL
  database_signature <- paste(df[1,], collapse=" ")

  if(any(colnames(df)=="Number.Of.Volumes")){database <- "Zotero_unknown"
  } else if(any(stringr::str_detect(as.character(database_signature), as.character(synthesisr::databases$signature)))){
    database <- synthesisr::databases$database[which(stringr::str_detect(as.character(database_signature), as.character(synthesisr::databases$signature)))]
  } else {database <- "Unknown"}

  database <- as.character(database)

  return(database)

}

#' Import results of a systematic review
#' @description Given a file or directory, imports and assembles search results
#' @param directory a path to a directory containing search results to import
#' @param filename a path to a filename containing search results to import
#' @param save_dataset if TRUE, saves the full search results to a .csv
#' @param save_directory the path to a directory where search results will be saved if save_dataset is set to TRUE
#' @param verbose if TRUE, prints status updates
#' @return a data frame of assembled search results
import_results <- function(directory=NULL, filename=NULL, save_dataset = FALSE, save_directory="./", verbose = TRUE){
  if(save_dataset==TRUE){
    if(utils::menu(c("yes", "no"),
                   title="This will save the full imported dataset in the specified directory. Are you sure?")==2){
      save_dataset <- FALSE
    }
  }

  if(!is.null(directory)){import_files <- paste(directory, list.files(path = directory), sep = "")} else if(!is.null(filename)){import_files <- filename}
  if(is.null(directory) & is.null(filename)){stop("No input given. Either directory or filename needs to be provided.")}

  import_files <- synthesisr::check_filetypes(import_files)

  for(i in 1:length(import_files)){
    filename <- import_files[i]

    df <- synthesisr::read_files(filename)

    # I can't remember why this cleaning step is here, but I'm sure something broke without it
      if (stringr::str_detect(paste(colnames(df), collapse=" "), "\\.\\.")){
      temp_cn <- strsplit(as.character(colnames(df)[1]), "\\.\\.")
      if (length(temp_cn[[1]]) > 1) {
        dotremoved <- gsub("\\.", "", temp_cn[[1]][2])
        colnames(df)[1] <- dotremoved
      }
    }
    if(any(colnames(df)=="X")){df <- df[, -which(colnames(df)=="X")]}

    if(verbose==TRUE){print(paste("Importing file", import_files[i]))}

    if(any(c("bib", "nbib", "ris")==synthesisr::detect_filetype(import_files[i]))){
      database <- "revtools"
    } else {
      database <- synthesisr::detect_database(df)
    }

    if(database!="Unknown"){
    import_function <- as.character(synthesisr::databases$import_function[which(synthesisr::databases$database==database)])
    import_function <- eval(parse(text=import_function))
    df <- import_function(df)
    }

    if (database != "Unknown") {
      df$database <- rep(database, nrow(df))
      df <- standardize_df(df)

      if (i == 1) {
        search_hits <- df
      }
      if (i > 1) {
        search_hits <- rbind(search_hits, df)
      }
    }

    if(database=="Unknown"){
      print(paste("Warning: Unable to recognize format for", import_files[i]))
    }
  }

  if (save_dataset == TRUE) {
    write.csv(search_hits, paste(save_directory, "full_dataset.csv", sep=""))
    print("Complete dataset written to .csv file.")
  }
  return(search_hits)
}






