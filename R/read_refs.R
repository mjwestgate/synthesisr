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
    null_check <- unlist(lapply(result_list, is.null))
    if(any(null_check)){
      result_list <- result_list[-which(null_check)]
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

  }else{ # i.e. if onely one filename given
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

  if(verbose){cat(paste0("Reading file ", filename, " ... "))}
  x <- readLines(filename, warn = FALSE)

<<<<<<< HEAD
  if(!inherits(df, "data.frame") & return_df){
    df <- as.data.frame(df)
    df <- synthesisr::clean_df(df)
  }
=======
  parse_function <- detect_format(x[1:min(c(length(x), 200))])
>>>>>>> 4164f915b33f617b23397c61a6f20ae795e526cc

  if(parse_function != "unknown"){

    df <- do.call(parse_function, list(x = x))

    if(!inherits(df, "data.frame") & return_df){
      df <- as.data.frame(df)
      df <- synthesisr::clean_df(df)
    }

    if(verbose){cat("done\n")}

    return(df)
  }else{
<<<<<<< HEAD
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


#' Parse medline-formatted ris files
#'
#' @description Imports files using medline ris format.
#' @param x A character vector containing bibliographic information in medline ris format.
#' @return Returns an object of class bibliography.
#' @example inst/examples/read_medline.R
parse_medline <- function(x){

  x <- prep_ris(x, detect_delimiter(x))

  x_merge <- merge(x,
    code_lookup, # tag_lookup(type = "medline"),
    by.x = "ris",
    by.y = "code",
    all.x = TRUE,
    all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$row_order), ]
  x_merge$description <- tolower(x_merge$description)

  # convert into a list, where each reference is a separate entry
  x_split <- split(x_merge[c("description", "text")], x_merge$ref)
  x_final <- lapply(x_split, function(a){
    result <- split(a$text, a$description)
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

  x <- prep_ris(x, synthesisr::detect_delimiter(x))

  # merge data with lookup info, to provide bib-style tags
  x_merge <- merge(x,
    synthesisr::code_lookup, # tag_lookup(type = "medline"),
    by.x = "ris",
    by.y = "code",
    all.x = TRUE,
    all.y = FALSE
  )
  x_merge <- x_merge[order(x_merge$row_order), ]
  x_merge$description <- tolower(x_merge$description)

  # find a way to store missing .bib data rather than discard
  if(any(is.na(x_merge$description))){
    rows_tr <- which(is.na(x_merge$description))
    x_merge$description[rows_tr] <- x_merge$ris[rows_tr]
    if(all(is.na(x_merge$row_order))){
      start_val <- 0
    }else{
      start_val <- max(x_merge$row_order, na.rm = TRUE)
    }
    x_merge$row_order[rows_tr] <- as.numeric(
      as.factor(x_merge$ris[rows_tr])
    ) + start_val
=======
    warning(paste("file type not recognised for ", filename, " - skipping"))
>>>>>>> 4164f915b33f617b23397c61a6f20ae795e526cc
  }

}