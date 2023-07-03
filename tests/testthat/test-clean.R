test_that("clean_authors() works", {
  author_list <-
    c(
      "Haddaway, N.R., A. Feirman, M.J. Grainger, C.T. Gray, E. Tanriver-Ayder, S. Dhaubanjar, & M.J Westgate",
      "Grames, E.M., A.N. Stillman, M.W. Tingley AND C.S. Elphick"
    )
  expect_false(any(grepl("&", clean_authors(author_list))))
  expect_false(any(grepl("AND", clean_authors(author_list))))
})

test_that("clean_colnames() works", {
  cleaned <- clean_colnames(c(".title...", "X..YEAR",
                              "authors..", ".AUTHOR"))
  expect_false(any(grepl("[[:punct::]]", cleaned)))
  expect_false(any(duplicated(cleaned)))
  expect_false(any(grepl("^(X|Y|Z)\\.+", cleaned)))
})
