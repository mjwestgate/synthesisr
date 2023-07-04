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
