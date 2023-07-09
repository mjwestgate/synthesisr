test_that("read_ref() works for simple imports", {
  df <- read_refs("testdata/eviatlas.txt",
                  return_df = TRUE,
                  verbose = FALSE)
  expect_true(inherits(df, c("tbl", "data.frame")))
  expect_equal(nrow(df), 1)
  expect_true(any(grep("EviAtlas", df[1, ])))
})

test_that("read_refs() works for simple imports", {
  testfiles <- paste0("testdata/",
                      c("eviatlas.txt", "litsearchr.txt", "res_synth_methods.txt"))
  df <- read_refs(testfiles,
                  return_df = TRUE,
                  verbose = FALSE)
  expect_true(inherits(df, c("tbl", "data.frame")))
  expect_equal(nrow(df), 4)
  expect_true(any(grep("EviAtlas", df[1, ])))
  expect_true(any(grep("litsearchr", df[2, ])))
  expect_true(any(grep("robvis", df[3, ])))
})

test_that("pubmed formats are read correctly", {
  x <- read_refs("testdata/PubMed_example.txt")
})

# test_that("read_refs() imports special characters correctly", {
# })

test_that("read_refs() stores multi-value fields as list columns", {
  df <- read_refs("testdata/Scopus_ris_example.ris",
                  return_df = FALSE,
                  verbose = FALSE)
  result <- as_tibble(df)
  # test goes here
})

test_that("bibtex imports properly with json code", {
  x <- read_ref("testdata/Scopus_bib_example.bib")
  expect_true(inherits(x, c("data.frame", "tbl")))
  expect_equal(nrow(x), 3)
})
