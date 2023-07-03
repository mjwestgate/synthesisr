test_that("format_citation() works for an object of class bibliography", {
  bib <- read_refs("testdata/eviatlas.txt", return_df = FALSE)
  expect_equal(
    format_citation(bib)[[1]],
    "Haddaway, Neal R. et al. (2019) Eviatlas: a Tool for Visualising Evidence Synthesis Databases. Environmental Evidence.")
})

test_that("format_citation() works for an object of class data.frame", {
  df <- read_refs("testdata/eviatlas.txt", return_df = TRUE)
  expect_equal(
    as.character(format_citation(df[1, ])),
    "Haddaway, Neal R. et al. (2019) Eviatlas: a Tool for Visualising Evidence Synthesis Databases. Environmental Evidence.")
})

test_that("format_citation() gives same result from .bib and data.frame", {
  bib <- read_refs("testdata/eviatlas.txt", return_df = FALSE)
  df <- read_refs("testdata/eviatlas.txt", return_df = TRUE)
  expect_equal(format_citation(df), format_citation(bib))
})

test_that("add_line_breaks() works as expected", {
  title <-
    "On the Origin of Species by Means of Natural Selection, or the Preservation of Favoured Races in the Struggle for Life"
  n <- 20
  lines_added <- add_line_breaks(title, n = n)
  split_text <- strsplit(lines_added, "\n")[[1]]
  expect_equal(length(split_text), 5)
  expect_true(all(unlist(lapply(split_text, nchar)) < n))
})
