test_that("detect_delimiter() works for ris", {
  lines <- readLines("testdata/eviatlas.txt")
  expect_equal(detect_delimiter(lines), "endrow")
})

test_that("detect_parser recognises files correctly", {
  file_names <- c("ASP_ris_example.ris", "citesource_issue_24.ris",
    "Ovid_ris_example.ris", "PubMed_example.txt", "Scopus_bib_example.bib",
    "Scopus_ris_example.ris", "WoS_ciw_example.ciw", "WoS_txt_example.txt")
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
