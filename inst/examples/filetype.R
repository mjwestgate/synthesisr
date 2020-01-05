files <-
  paste(system.file(package = "synthesisr"),
        "/extdata/zoorec.txt",
        sep = "")

detect_filetype(files)
