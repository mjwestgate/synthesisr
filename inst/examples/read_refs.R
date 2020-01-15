files <-
  paste(system.file(package = "synthesisr"),
        "/extdata/scopus.ris",
        sep = "")

results <- read_refs(files)
