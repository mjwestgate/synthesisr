#' Format a citation
#'
#' @description This function takes an object of class data.frame, list, or bibliography and returns a formatted citation.
#' @param data An object of class data.frame, list, or or bibliography.
#' @param details Logical: Should identifying information such as author names & journal titles be displayed? Defaults to TRUE.
#' @param abstract Logical: Should the abstract be shown (if available)? Defaults to FALSE.
#' @param add_html Logical: Should the journal title be italicized using html codes? Defaults to FALSE.
#' @param line_breaks Either logical, stating whether line breaks should be added, or numeric stating how many characters should separate consecutive line breaks. Defaults to FALSE.
#' @param ... any other arguments.
#' @return Returns a string of length equal to length(data) that contains formatted citations.
#' @example inst/examples/format_citation.R
format_citation <- function(
  data,
  details = TRUE,
  abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
  ...
){
  if(!(class(data) %in% c("data.frame", "bibliography", "list"))){
  stop(print("format_citation expects input data to be an object of class data.frame, bibliography, or list"))
    }

  if(class(data)!="data.frame"){
    data <- as.data.frame(data)
  }

  colnames(data) <- clean_colnames(colnames(data))
  if(any(names(data) == "journal")){
    source <- "journal"
  }else{
    source_check <- grepl("source", names(data))
    if(any(source_check)){
      source <- names(data)[which(source_check)]
      if(length(source) > 1){
        source <- source[which.max(nchar(data[source], type = "bytes"))]
      }
    }else{
      source <- NA
    }
  }

  # this section should be made more flexible to use any available information
  # if(details){
  data_list <- split(data, seq_len(nrow(data)))
  data_out <- unlist(lapply(data_list, function(a){
    cols_tr <- names(a)
    text_list <- as.list(rep(NA, 4))
    names(text_list) <- c("author", "year", "title", "journal")
    # title
    if(any(cols_tr == "title")){
      title_text <- tools::toTitleCase(tolower(a$title))
      if(grepl("[[:punct:]]$", title_text)){
        text_list$title <- title_text
      }else{
        text_list$title <- paste0(title_text, ".")
      }
    }else{
      text_list$title <- ""
    }
    if(details){
      # year
      if(any(cols_tr == "year")){
        text_list$year <- paste0("(", a$year, ")")
      }else{
        text_list$year <- NA
      }
      # journal
      if(!is.na(source)){
        if(!is.na(a[[source]])){
          journal_text <- tools::toTitleCase(tolower(a[[source]]))
          if(add_html){
            text_list$journal <- paste0("<i>", journal_text, "</i>. ")
          }else{
            text_list$journal <- paste0(journal_text, ". ")
          }
        }else{
          text_list$journal <- NA
        }
      }
      # authors
      if(any(cols_tr == "author")){
        author_vector <- strsplit(a[['author']], " and ")[[1]]
        if(length(author_vector) == 1){
          text_list$author <- a[['author']]
        }else{
          text_list$author <- paste0(author_vector[1], " et al.")
        }
      }else{
        if(!all(is.na(text_list))){
          text_list$author <- "Anon."
        }
      }
    } # end if(details)
    text_vec <- unlist(text_list)
    if(all(is.na(text_vec))){
      return(a[1])
    }else{
      return(
        paste(text_vec[!is.na(text_vec)], collapse = " ")
      )
    }
  }))

  # add line breaks if required
  if(is.logical(line_breaks)){
    if(line_breaks){
      data_out <- add_line_breaks(data_out)
    }
  }else{
    if(is.numeric(line_breaks)){
      data_out <- add_line_breaks(data_out, line_breaks)
    }
  }
  data_out <- unlist(lapply(data_out, trimws))
  return(data_out)
  }
