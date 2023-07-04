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

test_that("add_line_breaks() limits lines to supplied length", {
  title <-
    "On the Origin of Species by Means of Natural Selection, or the Preservation of Favoured Races in the Struggle for Life"
  lines_added <- add_line_breaks(title, n = 20)
  split_text <- strsplit(lines_added, "\n")[[1]]
  expect_equal(length(split_text), 8)
  expect_true(all(unlist(lapply(split_text, nchar)) <= 20))

  # and with higher n
  lines_added <- add_line_breaks(title, n = 40)
  split_text <- strsplit(lines_added, "\n")[[1]]
  expect_equal(length(split_text), 4)
  expect_true(all(unlist(lapply(split_text, nchar)) <= 40))
})

test_that("add_line_breaks() works on vectors", {
  titles <- c(
    "It is a truth universally acknowledged, that a single man in possession of a good fortune must be in want of a wife.",
    "No one would have believed in the last years of the nineteenth century that this world was being watched keenly and closely by intelligences greater than man’s and yet as mortal as his own"
  )
  lines_added <- add_line_breaks(titles, n = 50)
  string_lengths <- unlist(lapply(strsplit(lines_added, "\n"), nchar))
  expect_true(all(string_lengths <= 50))
  expect_equal(length(lines_added), 2)
})
