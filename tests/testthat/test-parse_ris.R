# parse_ris
# clean_ris_years
# clean_ris_authors

test_that("`get_tag_lookup()` errors appropriately", {
  # no data
  get_tag_lookup() |>
    expect_error(label = "`x` is missing, with no default")
  # wrong object type
  get_tag_lookup(list(x = 1)) |>
    expect_error(label = "Argument `x` must inherit from class `data.frame`")
  # no `ris` column
  tibble::tibble(x = 1) |>
    get_tag_lookup() |>
    expect_error(label = "Can't extract columns that don't exist")
  # wrong tag_naming argument
  get_tag_lookup(tibble::tibble(ris = c("AU", "PY", "TI")),
                 tag_naming = "nothankyou") |>
    expect_error(label = "argument `tag_naming` not recognized")
})

test_that("`parse_ris_abstract()` concatenates multiple strings", {
  x <- list(abstract = c("line 1", "line 2", "line 3"))
  result <- parse_ris_abstract(x)
  expect_equal(length(result), 1)
  expect_equal(result$abstract, "line 1 line 2 line 3")
})

test_that("`parse_ris_year()` extracts a 4-digit number when present", {
  x <- list(year = "a string containing the year 2025")
  result <- parse_ris_year(x)
  expect_equal(length(result), 1)
  expect_equal(result$year, "2025")
})

test_that("`parse_ris_year()` removes all content if no 4-digit numbers are given", {
  x <- list(year = "a string containing no year")
  result <- parse_ris_year(x)
  expect_equal(length(result), 1)
  expect_equal(result$year, "")
})

test_that("`parse_ris_title()` concatenates duplicate entries", {
  x <- list(title = c("The longest title ever...", "or is it?"))
  result <- parse_ris_title(x)
  expect_equal(length(result), 1)
  expect_equal(result$title, "The longest title ever... or is it?")
})

test_that("`parse_ris_title()` removes additional spaces and trailing stops", {
  x <- list(title = "My    amazing   paper.")
  result <- parse_ris_title(x)
  expect_equal(length(result), 1)
  expect_equal(result$title, "My amazing paper")
})

test_that("`parse_ris_journal()` removes duplicate entries", {
  x <- list(journal = c("Journal of Something", "JoS"))
  result <- parse_ris_journal(x)
  expect_equal(length(result), 2)
  expect_equal(names(result), c("journal", "journal_secondary"))
  expect_true(all(lengths(result) == 1))
})

test_that("`parse_ris_journal()` removes additional spaces and trailing stops", {
  x <- list(journal = "Journal    of   Something.")
  result <- parse_ris_journal(x)
  expect_equal(length(result), 1)
  expect_equal(result$journal, "Journal of Something")
})

test_that("`parse_ris_page_numbers()` works", {
  x <- list(name = "something",
            pages = c(99, 50))
  result <- parse_ris_page_numbers(x)
  expect_equal(length(result), 2)
  expect_equal(result$pages, "50-99")
})
