files <-
  paste(system.file(package = "synthesisr"),
        "/extdata/zoorec.txt",
        sep = "")
results <- import_refs(files)
