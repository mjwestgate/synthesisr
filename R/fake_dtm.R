fake_dtm <-
  function (x,
            stop_words,
            bigram_check = TRUE,
            bigram_quantile = 0.8,
            retain_empty_rows = FALSE,
            stem_collapse=TRUE) {
    if (!(class(x) %in% c("character", "data.frame"))) {
      stop("make_dtm only accepts arguments of class 'data.frame' or 'character'")
    }
    if (class(x) == "data.frame") {
      x <- apply(x, 1, function(a) {
        paste(a, collapse = " ")
      })
    }
    n <- length(x)
    if (missing(stop_words)) {
      stop_words <- stopwords::stopwords(source = "stopwords-iso")
    }
    else {
      stop_words <- unique(tolower(stop_words))
    }
    x <- tolower(x)
    x <- gsub(" - ", " ", x)
    x <- synthesisr::remove_punctuation(x, preserve_punctuation = "-")

    if (bigram_check) {
      ngrams <-  synthesisr::get_ngrams(x, min_freq=NULL, ngram_quantile = bigram_quantile)
      x <- synthesisr::replace_ngrams(x, synthesisr::get_ngrams(x))
      x <- synthesisr::remove_punctuation(x, preserve_punctuation = c("_", "-"))

      new_ngrams <- gsub(" ", "_",
             synthesisr::remove_punctuation(ngrams, preserve_punctuation = c("_", "-")))

      all_terms <-
        unique(unlist(
          lapply(x, synthesisr::get_tokens, language = "English")
        ))

    }else{
      all_terms <-
        unique(unlist(
          lapply(x, synthesisr::get_tokens, language = "English")
        ))
      }
    dtm <- synthesisr::create_dfm(x, all_terms, closure = "none")

      dfm <- list()
      length(dfm) <- 6
      names(dfm) <- c("i", "j", "v", "nrow", "ncol", "dimnames")
      locations <- function(z) {
        which(z > 0)
      }
      tmp <- apply(dtm, 1, locations)

      dfm$i <- rep(seq(1, length(tmp)), lapply(tmp, length))
      dfm$j <- as.numeric(unlist(tmp))
      dfm$v <- dtm[dtm > 0]
      dfm$nrow <- dim(dtm)[1]
      dfm$ncol <- dim(dtm)[2]

      dfm$dimnames$Docs <- synthesisr::generate_ids(x)
      dfm$dimnames$Terms <- all_terms

      class(dfm) <- "simple_triplet_matrix"
      ## all good to here!

      # making a new function to check stemmed duplicates
      if(stem_collapse){
        dfm <- merge_stems(dfm)
      }

      # gotta sort out the empty rows issue
      if(retain_empty_rows==FALSE){
        nil <- which(rowSums(as.matrix(dfm))==0)
        dfm$nrow <- dfm$nrow-length(nil)
        dfm$dimnames$Docs <- dfm$dimnames$Docs[-nil]
        dfm$i <- as.integer(as.factor(dfm$i))
        }
    return(dfm)
  }



merge_stems <- function(dfm) {
  if (!class(dfm) %in% c("simple_triplet_matrix")) {
    stop("merge_stems only accepts objects of class simple_triplet_matrix")
  }

  stem_terms <- SnowballC::wordStem(dfm$dimnames$Terms)
  lookup <- data.frame(
    initial_n = seq_along(dimnames(dtm)$Terms),
    initial = (dimnames(dtm)$Terms),
    stemmed = SnowballC::wordStem(dimnames(dtm)$Terms),
    stringsAsFactors = FALSE
  )
  dtm_df <- data.frame(i = dfm$i, j = dfm$j, v = dfm$v)

  if (base::anyDuplicated(lookup$stemmed) > 0) {
    lookup$n <- nchar(lookup$initial)
    text_split <-
      split(lookup[, c("initial_n", "n")], lookup$stemmed)
    text_match <- data.frame(initial_n = unlist(lapply(text_split,
                                                       function(a) {
                                                         a$initial_n
                                                       })),
                             final_n = unlist(lapply(text_split, function(a) {
                               if (nrow(a) > 1) {
                                 rep(a$initial_n[order(a$n, decreasing = FALSE)[1]],
                                     nrow(a))
                               }
                               else {
                                 a$initial_n
                               }
                             })))
    lookup$final_n <-
      text_match$final_n[order(text_match$initial_n)]
    dtm_df$j_new <- lookup$final_n[dtm_df$j]
    dtm_list <- split(dtm_df[c("j_new", "v")], dtm_df$i)
    name_lookup <- as.numeric(names(dtm_list))
    dtm_df2 <- do.call(rbind, lapply(seq_along(dtm_list),
                                     function(a, data) {
                                       result <- unlist(lapply(split(data[[a]]$v, data[[a]]$j_new),
                                                               sum))
                                       return(data.frame(
                                         i = a,
                                         j = as.numeric(names(result)),
                                         v = result
                                       ))
                                     }, data = dtm_list))

    unique_j <- sort(unique(lookup$final_n))
    lookup2 <- data.frame(index = seq_len(max(lookup$final_n)),
                          end = NA)
    lookup2$end[unique_j] <- seq_along(unique_j)
    dfm$i <- name_lookup[dtm_df2$i]
    dfm$j <- lookup2$end[dtm_df2$j]
    dfm$dimnames$Terms <-
      lookup$initial[sort(unique(lookup$final_n))]
    dfm$v <- dtm_df2$v
    dfm$ncol <- length(unique(dfm$j))
  }

  return(dfm)
}
