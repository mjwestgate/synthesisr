#' Import bibliographic search results
#'
#' @description Imports common bibliographic reference formats (i.e. .bib, .ris, or .txt).
#' @param filename A path to a filename or vector of filenames containing search results to import.
#' @param return_df If TRUE, returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates.
#' @return Returns a data.frame or list of assembled search results.
#' @example inst/examples/import_refs.R
read_refs <- function(
  filename,
  return_df = TRUE,
  verbose = TRUE
){

  # the following code is an exact duplicate of 'read_bibliography'
  # It allows import of a single file or multiple files.
  invisible(Sys.setlocale("LC_ALL", "C"))
  on.exit(invisible(Sys.setlocale("LC_ALL", "")))

  if(missing(filename)){
    stop("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if(any(!file_check)){
    stop("file not found")
  }

  if(length(filename) > 1){
    result_list <- lapply(filename, function(a, df){
      synthesisr::read_ref(a, df)
    },
    df = return_df
    )
    names(result_list) <- filename

  # drop any unrecognized file types
  if(any(unlist(lapply(result_list, is.null)))){
    result_list <- result_list[-which(unlist(lapply(result_list, is.null)))]
  }

    if(return_df){
      result <- synthesisr::merge_columns(result_list)
      result$filename <- unlist(
        lapply(seq_len(length(result_list)),
        function(a, data){
          rep(names(data)[a], nrow(data[[a]]))
        },
        data = result_list
      ))
      if(any(colnames(result) == "label")){
        result$label <- make.unique(result$label)
      }
      return(result)
    }else{
      result <- do.call(c, result_list)
      return(result)
    }
  }else{
    return(
      synthesisr::read_ref(filename, return_df)
    )
  }
}

#' Internal function called by import_refs for each file
#'
#' @description This is the underlying workhorse function that imports bibliographic files; primarily intended to be called from import_refs.
#' @param filename A path to a filename containing search results to import.
#' @param return_df If TRUE, returns a data.frame; if FALSE, returns a list.
#' @param verbose If TRUE, prints status updates.
#' @return Returns a data.frame or list of assembled search results.
#' @example inst/examples/import_refs.R
read_ref <- function(
  filename,
  return_df = TRUE,
  verbose = TRUE
	){

  # start by detecting file extension
  ## converted this to a function that also wraps up detecting bib vs ris and ignoring unrecognizable things

  file_type <- synthesisr::detect_filetype(filename)

  if(file_type == "unknown"){
    stop("File type not recognized")
  }else{

    ## if it is something obvious, then import right away
    if(file_type == "bib"){
      df <- synthesisr::parse_bib(readLines(filename))
      # df <- synthesisr::as.data.frame.bibliography(df)
    }

    if(file_type == "txt"){
      df <- read.delim(filename, row.names = NULL)
      df <- synthesisr::match_columns(df)
      df <- synthesisr::clean_df(df)
    }

    ## if it is ris, then clean and prep it first
    if(file_type == "ris"){
      z <- readLines(filename)

      if(length(which(grepl("PMID", z))) > 0){
        df <- parse_medline(z)
      }else{
        df <- parse_ris(file_type)
      }
    }

    if(!inherits(df, "data.frame") & return_df){
      df <- synthesisr::as.data.frame(df)
    }

    return(df)
  }

}

#' Detects file types
#'
#' @description Because different file types require different import strategies, this function detects the input file type.
#' @param filename A path to a file.
#' @return Returns a character vector with the likely file type based on the file extension and content.
#' @example inst/examples/filetype.R
detect_filetype <- function(filename) {
  file_type <- ""
  file_extension_lookup <- regexpr(".[[:alnum:]]{2,}$", filename)
  file_type <-
    substr(filename, file_extension_lookup + 1, nchar(filename))

  if(file_type == "nbib") {
    file_type <- "ris"
  }

  if(!any(c("ris", "bib") == file_type)) {
    if(ncol(read.delim(filename, sep = "\t", row.names = NULL)) == 1) {
      file_type <- synthesisr::detect_format(readLines(filename))
    }else{
      file_type <- "txt"
    }
  }
  if(!any(c("ris", "bib", "txt") == file_type)) {
    file_type <- "unknown"
    print(paste("Note: file type for", filename, "not recognized."))
  }
  return(file_type)
}

#' Detects if a file is bib-like or ris-like
#'
#' @description Because bibliographic data  can be stored in multiple file types, this function determines if the format of text is more bib-like or ris-like.
#' @param x A character vector containing bibliographic data.
#' @return Returns the format of a file: either bib, ris, or unknown.
#' @example inst/examples/bibvris.R
detect_format <- function(x){
  nrows <- min(c(200, length(x)))
  n_brackets <- length(grep("\\{", x))
  n_dashes <- length(grep(" - ", x))
  if(n_brackets > nrows/3 | n_dashes>nrows/3){
    if(n_brackets>n_dashes){
      file_type <- "bib"
    }else{file_type <- "ris"}
  }else{file_type <- "unknown"}
  return(file_type)
}

#' Matches imported data to reference codes
#'
#' @description Takes an imported data.frame and rearranges it to match lookup codes.
#' @param df A data.frame that contains bibliographic information.
#' @return Returns a data.frame rearranged and coded to match standard bibliographic fields, with unrecognized fields appended.
#' @example inst/examples/match_columns.R
match_columns <- function(df){
  # figure out which columns match known tags
  hits <- as.numeric(match(synthesisr::code_lookup$code, colnames(df)))

  # rearrange data in standard(ish) order
  if(any(is.na(hits))){
    newdat <- df[, hits[!is.na(hits)]]
  }else{newdat <- df[,hits]}

  # retain columns even if they did not match lookup
  newcolnames <- synthesisr::code_lookup$field[match(colnames(newdat), synthesisr::code_lookup$code)]
  newcolnames[which(is.na(newcolnames) | newcolnames=="")] <- colnames(newdat)[which(is.na(newcolnames) | newcolnames=="")]
  colnames(newdat) <- newcolnames

  # drop duplicate columns that matched more than one tag
  newdat <- newdat[,-which(duplicated(colnames(newdat)))]

  return(newdat)
}


#' Remove factors from an object
#'
#' @description This function converts factors to characters to avoid errors with levels.
#' @param z A data.frame
#' @return Returns the input data.frame with all factors converted to character.
#' @examples remove_factors(list(as.factor(c("a", "b"))))
remove_factors <- function(z){
  z[] <- lapply(z, function(x){
    if(is.factor(x)){as.character(x)}else{x}
  })
  return(z)
}

# This function computes the rolling sum of detections; intended for use in detect_delimiter.
rollingsum <- function(a, n = 2L){
  tail(cumsum(a) - cumsum(c(rep(0, n), head(a, -n))), -n + 1)
}

#' Detect delimiter type in bibliographic files
#'
#' @description The delimiter in bibliographic files is often an endrow, a special character, or a space. This function detects which delimiter, if any, a file uses.
#' @param x A character vector containing bibliographic data.
#' @return Returns the delimiter type used in a file.
#' @example inst/examples/detect_delimiter.R
detect_delimiter <- function(x){
  if(any(grepl("^ER", x))){
    delimiter <- "endrow"
  }else{
    # special break: same character repeated >6 times, no other characters
    char_list <- strsplit(x, "")
    char_break_test <- unlist(
      lapply(char_list,
             function(a){length(unique(a)) == 1 & length(a > 6)}
      )
    )
    if(any(char_break_test)){
      delimiter <- "character"
    }else{
      # use space as a ref break (last choice)
      space_break_check <- unlist(lapply(
        char_list,
        function(a){all(a == "" | a == " ")}
      ))
      if(any(space_break_check)){
        delimiter <- "space"
      }else{
        stop("import failed: unknown reference delimiter")
      }
    }
  }
  return(delimiter)
}

# Clean an ris file for import
prep_ris <- function(
  z,
  delimiter
){
  # detect tags
  tags <- regexpr(
    "^([[:upper:]]{2,4}|[[:upper:]]{1}[[:digit:]]{1})\\s{1,}-\\s{0,}",
    perl = TRUE,
    z
  )
  z_dframe <- data.frame(
    text = z,
    row = seq_along(z),
    match_length = attr(tags, "match.length"),
    stringsAsFactors = FALSE
  )
  z_list <- split(z_dframe, z_dframe$match_length)
  z_list <- lapply(z_list, function(a){
    n <- a$match_length[1]
    if(n < 0){
      result <- data.frame(
        ris = "",
        text = a$text,
        row_order = a$row,
        stringsAsFactors = FALSE
      )
    }else{
      result <- data.frame(
        ris = sub("\\s{1,}-\\s{0,}", "", substr(a$text, 1, n)),
        text = gsub("^\\s+|\\s+$", "", substr(a$text, n+1, nchar(a$text))),
        row_order = a$row,
        stringsAsFactors = FALSE
      )
    }
    return(result)
  })
  z_dframe <- do.call(rbind, z_list)
  z_dframe <- z_dframe[order(z_dframe$row), ]

  # replace tag information for delimiter == character | space
  if(delimiter == "character"){ # i.e. a single character repeated many times
    z_dframe$ris[which(
      unlist(lapply(
        strsplit(z, ""),
        function(a){
          length(unique(a)) == 1 & length(a > 6)
        }
      ))
    )] <- "ER"
  }
  if(delimiter == "space"){
    z_dframe$ris[which(z_dframe$ris == "" & z_dframe$text == "")] <- "ER"
    # ensure multiple consecutive empty rows are removed
    z_rollsum <- rollingsum(z_dframe$ris == "ER")
    if(any(z_rollsum > 1)){
      z_dframe <- z_dframe[which(z_rollsum <= 1), ]
    }
  }
  if(delimiter == "endrow"){
    # work out what most common starting tag is
    z_dframe$ref <- c(0, cumsum(z_dframe$ris == "ER")[
      seq_len(nrow(z_dframe)-1)]
    ) # split by reference

    start_tags <- unlist(lapply(
      split(z_dframe$ris, z_dframe$ref),
      function(a){a[which(a != "")[1]]}
    ))
    start_tag <- names(which.max(xtabs(~ start_tags )))

    # continue old code
    row_df <- data.frame(
      start = which(z_dframe$ris == start_tag),
      end = which(z_dframe$ris == "ER")
    )
    z_list <- apply(
      row_df,
      1,
      function(a){c(a[1]:a[2])}
    )
    z_list <- lapply(
      z_list,
      function(a, lookup){lookup[a, ]},
      lookup = z_dframe
    )
    z_dframe <- as.data.frame(
      do.call(rbind, z_list)
    )
  }

  # cleaning
  z_dframe$ref <- c(0, cumsum(z_dframe$ris == "ER")[
    seq_len(nrow(z_dframe)-1)]
  ) # split by reference
  z_dframe <- z_dframe[which(z_dframe$text != ""), ] # remove empty rows
  z_dframe <- z_dframe[which(z_dframe$ris != "ER"), ] # remove end rows

  # fill missing tags
  z_split <- split(z_dframe, z_dframe$ref)
  z_split <- lapply(z_split, function(a){
    if(a$ris[1] == ""){
      a$ris[1] <- "ZZ"
    }
    accum_ris <- Reduce(c, a$ris, accumulate = TRUE)
    a$ris <- unlist(lapply(
      accum_ris,
      function(b){
        good_vals <- which(b != "")
        b[good_vals[length(good_vals)]]
      }))
    return(a)
  })
  z_dframe <- as.data.frame(
    do.call(rbind, z_split)
  )

  return(z_dframe)
}


#' Parse medline-formatted ris files
#'
#' @description Imports files using medline ris format.
#' @param x A character vector containing bibliographic information in medline ris format.
#' @return Returns an object of class bibliography.
#' @example inst/examples/read_medline.R
parse_medline <- function(x){

  x <- prep_ris(x)
  names(x)[3] <- "order"
  x_merge <- merge(x,
                   tag_lookup(type = "medline"),
                   by = "ris",
                   all.x = TRUE,
                   all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$order), ]

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("bib", "text")], x_merge$ref)
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$bib)
    if(any(names(result) == "abstract")){
      result$abstract <- paste(result$abstract, collapse = " ")
    }
    if(any(names(result) == "title")){
      if(length(result$title) > 1){
        result$title <- paste(result$title, collapse = " ")
      }
    }
    if(any(names(result) == "term_other")){
      names(result)[which(names(result) == "term_other")] <- "keywords"
    }
    if(any(names(result) == "date_published")){
      result$year <- substr(result$date_published, start = 1, stop = 4)
    }
    if(any(names(result) == "article_id")){
      doi_check <- grepl("doi", result$article_id)
      if(any(doi_check)){
        result$doi <- strsplit(result$article_id[which(doi_check)], " ")[[1]][1]
      }
    }
    return(result)
  })

  names(x_final) <- unlist(lapply(x_final, function(a){a$pubmed_id}))
  class(x_final) <- "bibliography"
  return(x_final)
}

#' Generate unique labels for entires
#'
#' @description Creates a unique label for each bibliographic entry using as much author and year data as possible.
#' @param x A list of bibliographic entires.
#' @return Returns a character vector of unique names.
#' @example inst/examples/generate_ids.R
generate_ids <- function(x){
  nonunique_names <- unlist(lapply(x, function(a){
    name_vector <- rep("", 3)
    if(any(names(a) == "author")){
      name_vector[1] <- strsplit(a$author[1], ",")[[1]][1]
    }
    if(any(names(a) == "year")){
      name_vector[2] <- a$year[1]
    }
    if(any(names(a) == "journal")){
      journal_info <- strsplit(a$journal, " ")[[1]]
      name_vector[3] <- paste(
        substr(journal_info, 1, min(nchar(journal_info), 4)),
        collapse = "")
    }
    name_vector <- name_vector[which(name_vector != "")]
    if(length(name_vector) == 0){
      return("ref")
    }else{
      return(paste(name_vector, collapse = "_"))
    }
  }))

  # where this is not possible, give a 'ref1' style result
  if(any(nonunique_names == "ref")){
    rows_tr <- which(nonunique_names == "ref")
    nonunique_names[rows_tr] <- create_index("ref", length(rows_tr))
  }

  # ensure names are unique
  if(length(unique(nonunique_names)) < length(nonunique_names)){
    nonunique_names <- make.unique(nonunique_names, sep = "_")
  }

  return(nonunique_names)
}


#' Read ris files
#'
#' @description Imports files using ris format.
#' @param x A character vector containing bibliographic information in ris format.
#' @return Returns an object of class bibliography.
#' @example inst/examples/read_medline.R
parse_ris <- function(x){

  x <- prep_ris(x)

  # merge data with lookup info, to provide bib-style tags
  x_merge <- merge(x, tag_lookup(type = "ris"),
                   by = "ris",
                   all.x = TRUE,
                   all.y = FALSE)
  x_merge <- x_merge[order(x_merge$row_order), ]

  # find a way to store missing .bib data rather than discard
  if(any(is.na(x_merge$bib))){
    rows_tr <- which(is.na(x_merge$bib))
    x_merge$bib[rows_tr] <- x_merge$ris[rows_tr]
    if(all(is.na(x_merge$order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$order, na.rm = TRUE)
    }
    x_merge$order[rows_tr] <- as.numeric(
      as.factor(x_merge$ris[rows_tr])
    ) + start_val
  }

  # method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x_merge$text)
  if(any(year_check > 0)){
    check_rows <- which(year_check > 0)
    year_strings <- as.numeric(x_merge$text[check_rows])

    # for entries with a bib entry labelled year, check that there arent multiple years
    if(any(x_merge$bib[check_rows] == "year", na.rm = TRUE)){
      # check for repeated year information
      year_freq <- xtabs(~ ref, data = x_merge[which(x_merge$bib == "year"), ])
      if(any(year_freq > 1)){
        year_df <- x_merge[which(x_merge$bib == "year"), ]
        year_list <- split(nchar(year_df$text), year_df$ris)
        year_4 <- sqrt((4 - unlist(lapply(year_list, mean))) ^ 2)
        # rename bib entries that have >4 characters to 'year_additional'
        incorrect_rows <- which(
          x_merge$ris != names(which.min(year_4)[1]) &
            x_merge$bib == "year"
        )
        x_merge$bib[incorrect_rows] <- "year_additional"
      }
    }else{
      possible_rows <- which(
        year_strings > 0 &
          year_strings <= as.numeric(format(Sys.Date(), "%Y")) + 1
      )
      tag_frequencies <- as.data.frame(
        xtabs(~ x_merge$ris[check_rows[possible_rows]]),
        stringsAsFactors = FALSE
      )
      colnames(tag_frequencies) <- c("tag", "n")
      # now work out what proportion of each tag contain year data
      # compare against number of references to determine likelihood of being 'the' year tag
      tag_frequencies$prop <- tag_frequencies$n/(max(x_merge$ref)+1) # number of references
      if(any(tag_frequencies$prop > 0.9)){
        year_tag <- tag_frequencies$tag[which.max(tag_frequencies$prop)]
        rows.tr <- which(x_merge$ris == year_tag)
        x_merge$bib[rows.tr] <- "year"
        x_merge$order[rows.tr] <- 3
        # the following code was necessary when string >4 characters long were detected
        # x_merge$text[rows.tr] <- substr(
        #   x = x_merge$text[rows.tr],
        #   start = year_check[rows.tr],
        #   stop = year_check[rows.tr]+3
        # )
      }
    }
  }

  # use code from blog.datacite.org for doi detection
  # then return a consistent format - i.e. no www.dx.doi.org/ etc.
  # regexpr("/^10.d{4,9}/[-._;()/:A-Z0-9]+$/i", test) # original code
  # doi_check <- regexpr("/10.\\d{4,9}/", x_merge$text) # my version
  # if(any(doi_check > 0)){
  # 	check_rows <- which(doi_check > 0)
  # 	x_merge$bib[check_rows] <- "doi"
  # 	x_merge$order[check_rows] <- 11
  # 	x_merge$text[check_rows] <- substr(
  #     x = x_merge$text[check_rows],
  # 		start = doi_check[check_rows]+1,
  # 		stop = nchar(x_merge$text[check_rows])
  #   )
  # }

  # ensure author data from a single ris tag
  if(any(x_merge$bib == "author")){
    lookup.tags <- xtabs( ~ x_merge$ris[which(x_merge$bib == "author")])
    if(length(lookup.tags) > 1){
      replace_tags <- names(which(lookup.tags < max(lookup.tags)))
      replace_rows <- which(x_merge$ris %in% replace_tags)
      x_merge$bib[replace_rows] <- x_merge$ris[replace_rows]
      if(all(is.na(x_merge$order))){
        start_val <- 0
      }else{
        start_val <- max(x_merge$order, na.rm = TRUE)
      }
      x_merge$order[replace_rows] <- start_val + as.numeric(
        as.factor(x_merge$ris[replace_rows])
      )
    }
  }

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("bib", "ris", "text", "order")], x_merge$ref)

  # convert to list format
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$bib)
    # YEAR
    if(any(names(result) == "year")){
      if(any(nchar(result$year) >= 4)){
        year_check <- regexpr("\\d{4}", result$year)
        if(any(year_check > 0)){
          result$year <- substr(
            x = result$year[which(year_check>0)],
            start = year_check[1],
            stop = year_check[1]+3
          )
        }else{
          result$year <- ""
        }
      }else{
        result$year <- ""
      }
    }
    # TITLE
    if(any(names(result) == "title")){
      if(length(result$title) > 1){
        if(result$title[1] == result$title[2]){
          result$title <- result$title[1]
        }else{
          result$title <- paste(result$title, collapse = " ")
        }
      }
      result$title <- gsub("\\s+", " ", result$title) # remove multiple spaces
      result$title <- sub("\\.$", "", result$title) # remove final full stops
    }
    # JOURNAL
    if(any(names(result) == "journal")){
      unique_journals <- unique(result$journal)
      if(length(unique_journals)>1){
        unique_journals <- unique_journals[order(
          nchar(unique_journals),
          decreasing = FALSE
        )]
        result$journal <- unique_journals[1]
        result$journal_secondary <- paste(
          unique_journals[c(2:length(unique_journals))],
          collapse = "; "
        )
      }else{
        result$journal <- unique_journals
      }
      result$journal <-gsub("  ", " ", result$journal)
      result$journal <-sub("\\.$", "", result$journal)
    }
    # ABSTRACT
    if(length(result$abstract > 1)){
      result$abstract <- paste(result$abstract, collapse = " ")
      result$abstract <- gsub("\\s+", " ", result$abstract) # remove multiple spaces
    }
    # PAGE NUMBER
    if(any(names(result) == "pages")){
      if(length(result$pages) > 1){
        result$pages <- paste(sort(result$pages), collapse = "-")
      }
    }
    entry.order <- unlist(lapply(
      names(result),
      function(b, order){
        order$order[which(order$bib == b)[1]]
      },
      order = a
    ))
    final_result <- result[order(entry.order)]

    return(final_result)
  })

  names(x_final) <- generate_ids(x_final)
  class(x_final) <- "bibliography"
  return(x_final)
}



#' Read bib files
#'
#' @description This function reads in bibliographic data stored in .bib format.
#' @param x A character vector containing bibliographic information in bib format.
#' @return Returns an object of class bibliography.
#' @example inst/examples/read_bib.R
parse_bib <- function(x){

  # which lines start with @article?
  group_vec <- rep(0, length(x))
  row_id <- which(regexpr("^@", x) == 1)
  group_vec[row_id] <- 1
  group_vec <- cumsum(group_vec)

  # work out row names
  ref_names <- gsub(".*\\{|,$", "", x[row_id])
  ref_type <- gsub(".*@|\\{.*", "", x[row_id])

  # split by reference
  x_split <- split(x[-row_id], group_vec[-row_id])
  length_vals <- unlist(lapply(x_split, length))
  x_split <- x_split[which(length_vals > 3)]

  x_final <- lapply(x_split, function(z){

    # first use a stringent lookup term to locate only tagged rows
    delimiter_lookup <- regexpr(
      "^[[:blank:]]*([[:alnum:]]|[[:punct:]])+[[:blank:]]*=[[:blank:]]*\\{+",
      z
    )
    delimiter_rows <- which(delimiter_lookup != -1)
    other_rows <- which(delimiter_lookup == -1)
    delimiters <- data.frame(
      row = delimiter_rows,
      location = regexpr("=", z[delimiter_rows])
    )
    split_tags <- apply(delimiters, 1, function(a, lookup){
      c(
        row = as.numeric(a[1]),
        tag = substr(
          x = lookup[a[1]],
          start = 1,
          stop = a[2] - 1
        ),
        value = substr(
          x = lookup[a[1]],
          start = a[2] + 1,
          stop = nchar(lookup[a[1]])
        )
      )
    },
    lookup = z
    )
    entry_dframe <- rbind(
      as.data.frame(
        t(split_tags),
        stringsAsFactors = FALSE
      ),
      data.frame(
        row = other_rows,
        tag = NA,
        value = z[other_rows],
        stringsAsFactors = FALSE
      )
    )
    entry_dframe$row <- as.numeric(entry_dframe$row)
    entry_dframe <- entry_dframe[order(entry_dframe$row), c("tag", "value")]

    if(any(entry_dframe$value == "}")){
      entry_dframe <- entry_dframe[seq_len(which(entry_dframe$value == "}")[1]-1), ]
    }
    if(any(entry_dframe$value == "")){
      entry_dframe <- entry_dframe[-which(entry_dframe$value == ""), ]
    }

    # remove whitespace
    entry_dframe <- as.data.frame(
      lapply(entry_dframe, trimws),
      stringsAsFactors = FALSE
    )
    # remove 1 or more opening brackets
    entry_dframe$value <- gsub("^\\{+", "", entry_dframe$value)
    # remove 1 or more closing brackets followed by zero or more punctuation marks
    entry_dframe$value <- gsub("\\}+[[:punct:]]*$", "", entry_dframe$value)

    # convert each entry to a list
    label_group <- rep(0, nrow(entry_dframe))
    tag_rows <- which(entry_dframe$tag != "")
    label_group[tag_rows] <- 1
    tag_names <- entry_dframe$tag[tag_rows]
    entry_list <- split(
      entry_dframe$value,
      cumsum(label_group)+1
    )
    names(entry_list) <- tolower(
      gsub("^\\s+|\\s+$",  "", tag_names)
    )
    entry_list <- lapply(entry_list,
                         function(a){paste(a, collapse = " ")}
    )
    if(any(names(entry_list) == "author")){
      if(length(entry_list$author) == 1){
        entry_list$author <- strsplit(entry_list$author, " and ")[[1]]
      }
    }
    return(entry_list)
  })

  # add type
  x_final <- lapply(
    seq_len(length(x_final)),
    function(a, type, data){
      c(type = type[a], data[[a]])
    },
    type = ref_type,
    data = x_final
  )

  names(x_final) <- ref_names
  class(x_final) <- "bibliography"
  return(x_final)

}
