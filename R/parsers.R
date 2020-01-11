#' Clean an RIS file for import
#'
#' @description This function preps RIS files by cleaning common issues and converting to a common format.
#' @param z A character vector that contains RIS bibliographic information.
#' @param delimiter A string indicating the type of delimiter separating entries.
#' @return Returns a data.frame intended for import with read_ris.
prep_ris <- function(
  z,
  delimiter
){
  # detect tags
  tags <- regexpr(
    "^([[:upper:]]{2,4}|[[:upper:]]{1}[[:digit:]]{1})\\s{0,}-{0,2}\\s{0,}",
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
        ris = sub("\\s{0,}-\\s{0,}|^\\s+|\\s+$", "", substr(a$text, 1, n)),
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
    # This function computes the rolling sum of detections; intended for use in detect_delimiter.
    rollingsum <- function(a, n = 2L){
      tail(cumsum(a) - cumsum(c(rep(0, n), head(a, -n))), -n + 1)
    }

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
  z_dframe$text <- trimws(z_dframe$text)

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

#' @describeIn parse_ris Parse bibliographic text
parse_pubmed <- function(x){

  x <- prep_ris(x, detect_delimiter(x))

  x_merge <- merge(x,
                   synthesisr::code_lookup[synthesisr::code_lookup$ris_pubmed, c("code", "order", "field")],
    by.x = "ris",
    by.y = "code",
    all.x = TRUE,
    all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$row_order), ]

  # find a way to store missing .bib data rather than discard
  if(any(is.na(x_merge$field))){
    rows_tr <- which(is.na(x_merge$field))
    x_merge$field[rows_tr] <- x_merge$ris[rows_tr]

    # ensure all headings have an order
    if(all(is.na(x_merge$order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$order, na.rm = TRUE)
    }
    x_merge$order[rows_tr] <- as.numeric(as.factor(x_merge$ris[rows_tr])) + start_val
  }

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("field", "text", "order")], x_merge$ref)
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field)
    if(any(names(result) == "abstract")){
      result$abstract <- paste(result$abstract, collapse = " ")
    }
    if(any(names(result) == "address")){
      result$address <- strsplit(
        paste(result$address, collapse = " "),
        "\\.\\s"
      )[[1]]
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

    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    return(result[result_order])
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


#' Parse bibliographic text in a variety of formats
#'
#' @description Text in standard formats - such as imported via readLines(), for example - can be parsed using a variety of standard formats. Use detect_format() to determine which is the most appropriate parser for your situation.
#' @param x A character vector containing bibliographic information in ris format.
#' @return Returns an object of class bibliography.
#' @example inst/examples/parse_ris.R
parse_ris <- function(x){

  x <- prep_ris(x, detect_delimiter(x))

  # to avoid duplicate tags, decide whether to use wos or generic tags
  all_tags <- unique(x$ris)
  if(all(all_tags %in% synthesisr::code_lookup$code[synthesisr::code_lookup$ris_wos])){
    code_lookup_thisfile <- synthesisr::code_lookup[synthesisr::code_lookup$ris_wos, ]
  }else{
    code_lookup_thisfile <- synthesisr::code_lookup[synthesisr::code_lookup$ris_generic, ]
  }

  # merge data with lookup info, to provide bib-style tags
  x_merge <- merge(x,
    code_lookup_thisfile[, c("code", "order", "field")],
    by.x = "ris",
    by.y = "code",
    all.x = TRUE,
    all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$row_order), ]

  # find a way to store missing .bib data rather than discard
  if(any(is.na(x_merge$field))){
    rows_tr <- which(is.na(x_merge$field))
    x_merge$field[rows_tr] <- x_merge$ris[rows_tr]

    # ensure all headings have an order
    if(all(is.na(x_merge$order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$order, na.rm = TRUE)
    }
    x_merge$order[rows_tr] <- as.numeric(as.factor(x_merge$ris[rows_tr])) + start_val
  }

  # method to systematically search for year data
  year_check <- regexpr("^\\d{4}$", x_merge$text)
  if(any(year_check > 0)){
    check_rows <- which(year_check > 0)
    year_strings <- as.numeric(x_merge$text[check_rows])

    # for entries with a bib entry labelled year, check that there arent multiple years
    if(any(x_merge$field[check_rows] == "year", na.rm = TRUE)){
      # check for repeated year information
      year_freq <- xtabs(~ ref, data = x_merge[which(x_merge$field == "year"), ])
      if(any(year_freq > 1)){
        year_df <- x_merge[which(x_merge$field == "year"), ]
        year_list <- split(nchar(year_df$text), year_df$ris)
        year_4 <- sqrt((4 - unlist(lapply(year_list, mean))) ^ 2)
        # rename bib entries that have >4 characters to 'year_additional'
        incorrect_rows <- which(
          x_merge$ris != names(which.min(year_4)[1]) &
            x_merge$field == "year"
        )
        x_merge$field[incorrect_rows] <- "year_additional"
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
        x_merge$field[rows.tr] <- "year"
        x_merge$row_order[rows.tr] <- 3
      }
    }
  }

  # ensure author data from a single ris tag
  if(any(x_merge$field == "author")){
    lookup.tags <- xtabs( ~ x_merge$ris[which(x_merge$field == "author")])
    if(length(lookup.tags) > 1){
      replace_tags <- names(which(lookup.tags < max(lookup.tags)))
      replace_rows <- which(x_merge$ris %in% replace_tags)
      x_merge$field[replace_rows] <- x_merge$ris[replace_rows]
      if(all(is.na(x_merge$row_order))){
        start_val <- 0
      }else{
        start_val <- max(x_merge$row_order, na.rm = TRUE)
      }
      x_merge$row_order[replace_rows] <- start_val + as.numeric(
        as.factor(x_merge$ris[replace_rows])
      )
    }
  }

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("field", "ris", "text", "order")], x_merge$ref)

  # convert to list format
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$field)
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

    # ensure result is returned in the correct order
    result_order <- order(
      unlist(lapply(split(a$order, a$field), function(b){b[1]}))
    )
    return(result[result_order])
  })

  names(x_final) <- generate_ids(x_final)
  class(x_final) <- "bibliography"
  return(x_final)
}


#' @describeIn parse_ris Parse bibtex format
parse_bibtex <- function(x){

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

#' @describeIn parse_ris Parse comma-separated values
parse_csv <- function(x){
  match_columns(read.table(text = x, sep = ","))
}

#' @describeIn parse_ris Parse tab-separated values
parse_tsv <- function(x){
  match_columns(read.table(text = x, sep = "\t"))
}
