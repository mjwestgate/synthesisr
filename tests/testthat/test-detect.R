test_that("detect_delimiter() works for ris", {
  lines <- readLines("testdata/eviatlas.txt")
  expect_equal(detect_delimiter(lines), "endrow")
})

test_that("detect_parser recognises files correctly", {
  file_names <- list.files("testdata")
  file_names <- file_names[
    !grepl("eviatlas|litsearchr|res_synth_methods", file_names)]
  file_types <- lapply(file_names, function(a){
    x <- readLines(paste0("./testdata/", a), warn = FALSE)
    detect_parser(x)
  }) |>
    unlist()
  detected_formats <- sub("^parse_", "", file_types)
  expect_equal(
    c("ris", "ris", "ris", "pubmed", "bibtex", "ris", "ris", "ris"),
    detected_formats)
})
