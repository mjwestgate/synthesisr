test_that("standard bibtext is parsed correctly", {
  x <- readLines("./testdata/Scopus_bib_example.bib")
  result <- parse_bibtex(x)
  expect_equal(nrow(result), 3)
})

test_that("single-line entries don't break bibtex parsing", {
  bib <- "
@article{5,
  author = {Altman, Irwin and Haythorn, William},
  title = {The Effects of Social Isolation and Group Composition on Performance},
  journal = {Human Relations},
  publisher = {SAGE Publications},
  date = {1967-11},
  year = {1967},
  month = {11},
  pages = {313-340},
  volume = {20},
  number = {4},
  doi = {10.1177/001872676702000401}

@misc{6,
  author = {Williams, Katherine and Reilly, Charles and Ill}
}
"
result <- parse_bibtex(bib)
expect_equal(nrow(result), 2)
})
