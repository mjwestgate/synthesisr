files <-
  paste(system.file(package = "synthesisr"),
        "/extdata/scopus.ris",
        sep = "")

detect_filetype(files)
