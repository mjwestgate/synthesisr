test_that("write_refs() works", {
  lines <- read_refs("testdata/eviatlas.txt")
  evi_ris <- write_refs(lines, format = "ris", write = FALSE)
  evi_bib <- write_refs(lines, format = "bib", write = FALSE)
  expect_equal(detect_parser(evi_ris), "parse_ris")
  expect_equal(detect_parser(evi_bib), "parse_bibtex")
  expect_true(any(grep("ER ", evi_ris, ignore.case = FALSE)))
})

# Fix for Issue #22
test_that("write_refs() successfully writes from a tibble", {
  df <-  read_refs("testdata/eviatlas.txt")
  write_refs(df, "TEST_TIBBLE.bib")
  expect_true(file.exists("TEST_TIBBLE.bib"))
  unlink("TEST_TIBBLE.bib")
})
