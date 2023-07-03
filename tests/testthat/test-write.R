test_that("write_refs() works", {
  lines <- read_refs("testdata/eviatlas.txt")

  evi_ris <- write_refs(lines, format = "ris")
  evi_bib <- write_refs(lines, format = "bib")

  expect_equal(detect_parser(evi_ris), "parse_ris")
  expect_equal(detect_parser(evi_bib), "parse_bibtex")
  expect_true(any(grep("ER ", evi_ris, ignore.case = FALSE)))
})

