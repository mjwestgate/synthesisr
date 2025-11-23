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
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(x), 3)
  expect_equal(ncol(x), 37)
  expect_true(all(
    c("publication_type", "author", "journal", "title", "abstract") %in%
    colnames(x)))
})

# test_that("read_refs() imports special characters correctly", {
# })

# test_that("read_refs() stores multi-value fields as list columns", {
#   df <- read_refs("testdata/Scopus_ris_example.ris",
#                   return_df = FALSE,
#                   verbose = FALSE)
#   result <- as_tibble(df)
#   # test goes here
# })

test_that("bibtex imports properly with json code", {
  x <- read_ref("testdata/Scopus_bib_example.bib")
  expect_true(inherits(x, c("data.frame", "tbl")))
  expect_equal(nrow(x), 3)
})

test_that("write_refs() works", {
  lines <- read_refs("testdata/eviatlas.txt")
  evi_ris <- write_refs(lines, format = "ris", write = FALSE)
  evi_bib <- write_refs(lines, format = "bib", write = FALSE)
  expect_equal(detect_parser(evi_ris), "parse_ris")
  expect_equal(detect_parser(evi_bib), "parse_bibtex")
  expect_true(any(grep("ER ", evi_ris, ignore.case = FALSE)))
})

test_that("read-write-read roundtripping works for .ris files", {
  x <- read_refs("./testdata/citesource_issue_24.ris")
  dir.create("TEMP")
  write_refs(x, file = "TEMP/issue24.ris", format = "ris")
  expect_no_error({y <- read_refs("TEMP/issue24.ris")})
  expect_true(colnames(y)[1] == "source_type")
  expect_equal(nrow(x), nrow(y))
  # expect_equal(ncol(x), ncol(y)) # fails at present - i.e. round-tripping is lossy
  unlink("TEMP", recursive = TRUE)
})

test_that("`read_refs()` works for bibtext files with spaces around `=`", {
  litsearchr <- c(
    "@article{grames2019,
  title = {An automated approach to identifying search terms for systematic reviews using keyword co-occurrence networks},
  author={Grames, Eliza M and Stillman, Andrew N and Tingley, Morgan W and Elphick, Chris S},
  journal={Methods in Ecology and Evolution},
  volume={10},
  number={10},
  pages={1645--1654},
  year={2019},
  publisher={Wiley Online Library}
}"
  )
  tmp <- tempfile()
  writeLines(litsearchr, tmp)
  df <- read_refs(filename=tmp, return_df = TRUE, verbose = TRUE)
  expect_equal(ncol(df), 7)
  expect_equal(nrow(df), 1)
})
