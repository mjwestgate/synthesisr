#' Report search results per database
#' @description Reports how many hits were imported per database
#' @param df the data frame of search results
#' @param database_column the name or index of the column where database information is contained
#' @return a data frame with a count of results per database
report_results <- function(df, database_column){
  if(is.numeric(database_column) & database_column <= ncol(df)){target <- database_column
  } else if(any(colnames(df)==database_column)){
    target <- which(colnames(df)==database_column)
  } else{stop("The provided database column is not found in your data frame.")}

results <- as.data.frame(table(df[,target]))
colnames(results) <- c("Database", "Hits")
return(results)
}

#' Report accumulation of new results per database
#' @description Generates an accumulation curve of new unique hits per database
#' @param df the data frame of search results
#' @param database_column the name or index of the column where database information is contained
#' @param database_order a character vector of database names that appear in the database column, in the order that they should be added to the accumulation curve
#' @return a data frame with an accumulation of unique hits per unit of searching effort
report_accumulation <- function(df, database_column, database_order=NULL){

  counter <- 0

    if(is.numeric(database_column) & database_column <= ncol(df)){target <- database_column
  } else if(any(colnames(df)==database_column)){
    target <- which(colnames(df)==database_column)
  } else{stop("The provided database column is not found in your data frame.")}

  if(is.null(database_order)){
    database_order <- unique(df[,target])
  }

  for(i in 1:length(database_order)){
    results <- df[, which(target==database_order[i])]
  }

  # #################################################### #
  # #################################################### #
  # #### ELIZA DON'T FORGET TO FINISH THIS FUNCTION #### #
  # #################################################### #
  # #################################################### #
}
