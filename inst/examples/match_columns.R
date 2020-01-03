files <-
  paste(system.file(package = "synthesisr"),
        "/extdata/zoorec.txt",
        sep = "")

df <- read.delim(files)
formatted <- match_columns(df)
colnames(formatted)
