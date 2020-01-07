#' Format a citation
#'
#' @description This function takes an object of class data.frame or bibliography and returns a formatted citation.
#' @param data An object of class data.frame or bibliography.
#' @param details Logical: Should identifying information such as author names & journal titles be displayed? Defaults to TRUE.
#' @param abstract Logical: Should the abstract be shown (if available)? Defaults to FALSE.
#' @param add_html Logical: Should the journal title be italicized using html codes? Defaults to FALSE.
#' @param line_breaks Either logical, stating whether line breaks should be added, or numeric stating how many characters should separate consecutive line breaks. Defaults to FALSE.
#' @param ... any other arguments.
#' @return Returns a string of length equal to length(x) that contains formatted citations.
#' @example inst/examples/format_citation.R
format_citation <- function(
  data,
  details = TRUE,
  abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
  ...
){
  UseMethod("format_citation")
  }


#' @describeIn format_citation Format citation from a list
format_citation.list <- function(
	data, # list of data from a standard import function
  details = TRUE, # whether to allow or suppress bibliographic details - name, year, journal
	abstract = FALSE, # option to return only the citation for an article
  add_html = FALSE,
  line_breaks = FALSE,
  ...
){
	if(!details){
    result <- as.character(data["title"])
  }else{
		if(any(names(data) == "author")){
			# author info
			# remove any additional characters that display affiliations (i.e. those after last ".")
			author.data <- unlist(lapply(
        strsplit(as.character(data$author), ""),
        function(a){
  				dot.lookup <- a %in% "."
  				if(any(dot.lookup)){
            a <- a[seq_len(max(which(dot.lookup)))]
          }
  				return(paste(a, collapse = ""))
  			}
      ))
			if(any(grepl(",", author.data))){
				author.data <- unlist(lapply(
          strsplit(author.data, ", "),
          function(a){paste(a[2], a[1], sep = " ")}
        ))
			}
			n.authors <- length(data$author)
			if(n.authors >= 4){
        n.authors <- 4
      }
			author.info <- switch(as.character(n.authors),
				"0" = "Anon.",
				"1" = author.data,
				"2" = {paste(author.data, collapse = " & ")},
				"3" = {paste0(author.data[1], ", ", author.data[2], " & ", author.data[3])},
				"4" = {paste0(author.data[1], ", ", author.data[2], " et al.")})
		}
  # }
		# paste info in the correct order
		lookup.headers <- c("year", "title", "journal", "volume", "pages")
		lookup.result <- lookup.headers %in% names(data)
		if(all(lookup.result)){
			result <- paste0(
        author.info,
        " (", data$year, ") ",
				data$title, ". ",
        tools::toTitleCase(tolower(data$journal)),
        " ",
        data$volume,
        ": ",
        data$pages
      )
		}else{
			result <- paste(
        author.info,
        paste(data[lookup.headers[lookup.result]], collapse = " "),
        sep = " "
      )
    }
	  # note - the above doesn't add brackets around year
  }
	# add abstract if required
	if(abstract & any(names(data) == "abstract")){
		result <- paste0(result, ".<br><br><strong>Abstract</strong><br>", data$abstract)
	}

  if(is.logical(line_breaks)){
    if(line_breaks){
      result <- add_line_breaks(result)
    }
  }else{
    if(is.numeric(line_breaks)){
      result <- add_line_breaks(result, line_breaks)
    }
  }

return(result)
}

#' @describeIn format_citation Format citation from a bibliography
format_citation.bibliography <-  function(
	data,
  details = TRUE,
	abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
  ...
){
  lapply(data, function(a, details, abstract, add_html, line_breaks){
    format_citation.list(a, details, abstract, add_html, line_breaks)
    },
    details = details,
    abstract = abstract,
    add_html = add_html,
    line_breaks = line_breaks
  )
}

#' @describeIn format_citation Format citation from a data.frame
format_citation.data.frame <- function(
  data,
  details = TRUE,
  abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
  ...
){
  colnames(data) <- clean_names(colnames(data))
  if(any(names(data) == "journal")){
    source <- "journal"
  }else{
    source_check <- grepl("source", names(data))
    if(any(source_check)){
      source <- names(data)[which(source_check)[1]]
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
  return(data_out)
}

#' Add line breaks to one or more strings
#' @description This function takes a vector of strings and adds line breaks every n characters. Primarily built to be called internally by format_citation, this function has been made available as it can be useful in other contexts.
#' @param data Either a string or a vector; if the vector is not of class character if will be coerced to one using as.character.
#' @param n Numeric: The number of characters that should separate consecutive line breaks.
#' @details Line breaks are only added between words, so the value of n is acutally a threshold value rather than being matched exactly.
#' @return Returns the input vector unaltered except for the addition of line breaks.
#' @examples add_line_breaks(data=c("On the Origin of Species"), n=2)
add_line_breaks <- function(x, n = 50, html = FALSE){
  if(html){break_string <- "<br>"}else{break_string <- "\n"}
	split_text <- strsplit(as.character(x), " ")
  out_list <- lapply(split_text, function(a){
    if(length(a) == 0){
      return("")
    }else{
      result <- data.frame(
        text = a,
        nchars = nchar(a, allowNA = TRUE, keepNA = TRUE),
        stringsAsFactors = FALSE
      )
      if(any(is.na(result$nchars))){
        result$nchars[which(is.na(result$nchars))] <- 2
      }
    	result$sum <- cumsum(result$nchars)
    	result$group <- cut(result$sum,
    		breaks = seq(0, max(result$sum)+n-1, n),
    		labels = FALSE)
    	result_list <- split(result$text, result$group)
    	result <- paste(
        unlist(
          lapply(result_list, function(a){paste(a, collapse = " ")})
        ),
        collapse = break_string)
      return(result)
    }
  })
  return(unlist(out_list))
}
