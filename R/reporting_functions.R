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
