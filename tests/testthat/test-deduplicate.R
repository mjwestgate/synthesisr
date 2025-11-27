test_that("find_duplicates() doesn't flag list-col NULLs as identical", {
  result <- list(1, NULL, NULL, 2, 1) |>
    find_duplicates()
  expect_equal(as.integer(result),
               c(1, 2, 3, 4, 1))
})

test_that("find_duplicates() works", {
  # with no duplicates
  x <- read_refs("testdata/ASP_ris_example.ris")
  result <- x |>
    dplyr::pull("doi") |>
    find_duplicates()
  expect_equal(as.integer(result), c(1:4))

  # with duplicates
  x <- rbind(x, x[1,])
  result <- x |>
    dplyr::pull("doi") |>
    find_duplicates()
  expect_equal(as.integer(result), c(1:4,1))
})

test_that("`deduplicate()` works", {
  my_df <-  tibble::tibble(
    title = c(
      "EviAtlas: a tool for visualising evidence synthesis databases",
      "revtools: An R package to support article screening for evidence synthesis",
      "An automated approach to identifying search terms for systematic reviews using keyword co-occurrence networks",
      "Reproducible, flexible and high-throughput data extraction from primary literature: The metaDigitise r package",
      "eviatlas:tool for visualizing evidence synthesis databases.",
      "REVTOOLS a package to support article-screening for evidence synthsis"),
    year = c("2019", "2019", "2019", "2019", NA, NA),
    authors = c("Haddaway et al", "Westgate", "Grames et al", "Pick et al", NA, NA))

  # run deduplication on dataset with duplicates
  deduped <- deduplicate(my_df, "title",
                          rm_punctuation = TRUE,
                          to_lower = TRUE)
  expect_equal(deduped[1:3],my_df[1:4,])

  # run deduplicate on dataset without duplicates
  deduped_2 <- deduplicate(my_df[1:4,], "title",
                         rm_punctuation = TRUE,
                         to_lower = TRUE)
  expect_equal(deduped_2[1:3], my_df[1:4,])
})


