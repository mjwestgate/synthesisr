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

report_accumulation <- function(df, database_column, database_order){
  counter <- 0
  if(is.numeric(database_column) & database_column <= ncol(df)){target <- database_column
  } else if(any(colnames(df)==database_column)){
    target <- which(colnames(df)==database_column)
  } else{stop("The provided database column is not found in your data frame.")}

  for(i in 1:length(database_order)){
    results <- df[, which(target==database_order[i])]
  }
}
