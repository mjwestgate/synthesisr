# ensure multiple consecutive empty rows are removed
# This function computes the rolling sum of detections; intended for use in detect_delimiter.
rollingsum <- function(a, n = 2L){
  tail(cumsum(a) - cumsum(c(rep(0, n), head(a, -n))), -n + 1)
}


# ' Clean an RIS file for import
# '
# ' @description This function preps RIS files by cleaning common issues and converting to a common format.
# ' @param z A character vector that contains RIS bibliographic information.
# ' @param delimiter A string indicating the type of delimiter separating entries.
# ' @param type A string indicating the ris source; options are pubmed or generic.
# ' @return Returns a data.frame intended for import with parse_ris.
prep_ris <- function(
  z,
  delimiter,
  type # either "pubmed" or "generic". Not specified by user
){
  # detect tags
  if(type == "pubmed"){
    ris_regex <- "^[[:upper:]]{2,4}\\s*-\\s"
  }else{ # i.e. generic
    ris_regex <- "(^[[:upper:]]{2}|^[[:upper:]][[:digit:]])\\s*(-|:){0,2}\\s*"
  }
  tags <- regexpr(ris_regex, perl = TRUE, z)
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

  # clean up obvious errors
  z_dframe$ris <- gsub("[[:punct:]]", "", z_dframe$ris)
  z_dframe$text <- sub("^[[:punct:]]{0,1}\\s{2,}", "", z_dframe$text)

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

    # fix bug where not all entries start with same tag
    start_tag_xtab <- xtabs(~ start_tags )
    end_rows <- which(z_dframe$ris == "ER")
    # previous behavior:
    if(max(xtabs(~ start_tags)) == length(which(z_dframe$ris == "ER"))){
      start_tag <- names(which.max(xtabs(~ start_tags)))
      row_df <- data.frame(
        start = which(z_dframe$ris == start_tag),
        end = end_rows
      )
    # new option:
    }else{
      row_df <- data.frame(
        start = c(1, end_rows[1:(length(end_rows)-1)] - 1),
        end = end_rows
      )
    }

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
