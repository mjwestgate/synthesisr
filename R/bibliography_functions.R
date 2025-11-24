#' Methods for class bibliography
#'
#' @title bibliography-class
#' @description This is a small number of standard methods for interacting with class 'bibliography'. More may be added later.
#' @param x An object of class 'bibliography'
#' @param object An object of class 'bibliography'
#' @param n Number of items to select/print
#' @param ... Any further information
#' @aliases summary.bibliography, print.bibliography, c.bibliography, as.data.frame.bibliography
#' @name bibliography-class
#' @export
summary.bibliography <- function(object, ...){

  # are any abstracts completely missing?
  null_check <- unlist(lapply(
    object,
    function(a){is.null(a$abstract)}
  ))
  null_count <- length(object) - length(which(null_check))
  null_percent <- round((100/length(object)) * null_count, 1)

  # how many sources?
  sources <- unlist(lapply(
    object,
    function(a){a$journal}
  ))
  if(!is.null(sources)){
    n_sources <- length(unique(sources))
    source_freq <- sort(
      xtabs(~ sources),
      decreasing = TRUE
    )[seq_len(min(c(5, n_sources)))]
    # put text together
    result <- paste(
      paste0(
        "Object of class 'bibliography' containing ",
        length(object),
        " entries.",
        "\n  ",
        "Number containing abstracts: ",
        null_count,
        " (",
        null_percent,
        "%)",
        "\n",
        "Number of sources: ",
        n_sources,
        "\n",
        "Most common sources:",
        "\n  "
      ),
      paste(
        names(source_freq),
        " (n = ",
        as.numeric(source_freq),
        ")",
        sep = "",
        collapse = "\n  "
      ),
      sep = "",
      collapse = "\n")
  }else{
    result <- paste0(
      "Object of class 'bibliography' containing ",
      length(object),
      " entries.",
      "\n  ",
      "Number containing abstracts: ",
      null_count,
      " (",
      null_percent,
      "%)",
      "\n"
    )
  }
  cat(result, sep = "\n")
}

#' @rdname bibliography-class
#' @export
print.bibliography <- function(x, n, ...){
  length_tr <- length(x)
  if(missing(n)){
    n <- min(c(length_tr, 5))
  }else{
    if(n > length_tr){
      n <- length_tr
    }
  }
  text_tr <- format_citation(x[seq_len(n)])
  cat(paste(unlist(text_tr), collapse = "\n\n"))
}

#' @rdname bibliography-class
#' @export
'[.bibliography' <- function(x, n){
  class(x) <- "list"
  if(all(n %in% seq_len(length(x))) == FALSE){
    abort("subset out of bounds")
  }
  z <- x[n]
  class(z) <- "bibliography"
  return(z)
}

#' @rdname bibliography-class
#' @export
c.bibliography <- function(...){
  result <- lapply(list(...), function(a){
    class(a) <- "list"
    return(a)
  })
  result <- do.call(c, result)
  class(result) <- "bibliography"
  return(result)
}

#' @rdname bibliography-class
#' @export
as.data.frame.bibliography <- function(x, ...){
  as_tibble(x) |>
    as.data.frame()
  # NOTE: likely to break due to presence of list-columns (authors)
}


#' @rdname bibliography-class
#' @export
as.bibliography <- function(x, ...){

  if(!inherits(x, "data.frame")){
    abort("as.bibliography can only be called for objects of class 'data.frame'")
  }

  x_list <- lapply(
    split(x, seq_len(nrow(x))),
    function(a){
      a <- as.list(a)
      if(any(names(a) == "author")){
        a$author <- strsplit(a$author, " and ")[[1]]
      }
      if(any(names(a) == "keywords")){
        a$keywords <- strsplit(a$keywords, " and ")[[1]]
      }
      return(a)
    }
  )
  names(x_list) <- seq_len(nrow(x))
  class(x_list) <- "bibliography"
  return(x_list)
}

#' @rdname bibliography-class
#' @param .rows currently ignored
#' @param .name_repair currently ignored
#' @param rownames currently ignored
#' @export
as_tibble.bibliography <- function(x,
                                   ...,
                                   .rows,
                                   .name_repair,
                                   rownames){
  class(x) <- "list"
  as_tibble(purrr::list_transpose(x))
}
