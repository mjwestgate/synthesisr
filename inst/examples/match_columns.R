files <-
  paste(system.file(package = "synthesisr"),
        "/extdata/scopus.ris",
        sep = "")

df <- read.delim(files)
formatted <- match_columns(df)
colnames(formatted)
