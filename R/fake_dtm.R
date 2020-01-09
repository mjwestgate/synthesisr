fake_dtm <-
  function (x,
            stop_words,
            bigram_check = TRUE,
            bigram_quantile = 0.8,
            min_freq=NULL,
            stem_collapse=TRUE,
            language="English") {
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
      if(missing(language)){language <- "English"}
      stop_words <- synthesisr::get_stopwords(language = language)
    }
    else {
      stop_words <- unique(tolower(stop_words))
    }
    x <- tolower(x)
    x <- gsub(" - ", " ", x)
    x <- synthesisr::remove_punctuation(x, preserve_punctuation = "-")

    if (bigram_check) {
      #ngrams <-  unique(synthesisr::get_ngrams(x, min_freq=1))

      only_some <- function(entry){
        internal_ngrams <- synthesisr::get_ngrams(entry, min_freq = min_freq, ngram_quantile = bigram_quantile)
        synthesisr::replace_ngrams(entry, internal_ngrams)
      }

      x <- unlist(lapply(x, only_some))

      #x <- synthesisr::replace_ngrams(x, ngrams)
      #x <- sapply(ngrams, gsub, x=x, replacement="__")
      #x <- synthesisr::remove_punctuation(x, preserve_punctuation = c("_", "-"))

      #new_ngrams <- gsub(" ", "_",
      #       synthesisr::remove_punctuation(ngrams, preserve_punctuation = c("_", "-")))
    }

      x <- lapply(x, get_tokens, language="English")
      x <- unlist(lapply(x, paste, collapse=" "))

      dfm <- tm::DocumentTermMatrix(x = tm::Corpus(tm::VectorSource(tmpx2)),
                             control = list(wordLengths = c(4, Inf)))


      class(dfm) <- "simple_triplet_matrix"

      if(stem_collapse){
        dfm <- merge_stems(dfm)
      }

    return(dfm)
  }



merge_stems <- function(dfm) {
  if (!class(dfm) %in% c("simple_triplet_matrix")) {
    stop("merge_stems only accepts objects of class simple_triplet_matrix")
  }

  stem_terms <- SnowballC::wordStem(dfm$dimnames$Terms)
  lookup <- data.frame(
    initial_n = seq_along(dfm$dimnames$Terms),
    initial = (dfm$dimnames$Terms),
    stemmed = SnowballC::wordStem(dfm$dimnames$Terms),
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
