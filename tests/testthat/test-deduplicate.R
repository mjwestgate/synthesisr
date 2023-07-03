test_that("deduplicate works", {
  my_df <-  data.frame(
    title = c(
      "EviAtlas: a tool for visualising evidence synthesis databases",
      "revtools: An R package to support article screening for evidence synthesis",
      "An automated approach to identifying search terms for systematic reviews using keyword co-occurrence networks",
      "Reproducible, flexible and high-throughput data extraction from primary literature: The metaDigitise r package",
      "eviatlas:tool for visualizing evidence synthesis databases.",
      "REVTOOLS a package to support article-screening for evidence synthsis"),
    year = c("2019", "2019", "2019", "2019", NA, NA),
    authors = c("Haddaway et al", "Westgate", "Grames et al", "Pick et al", NA, NA),
    stringsAsFactors = FALSE)

  # run deduplication
  dups <- find_duplicates(my_df$title,
                          method = "string_osa",
                          rm_punctuation = TRUE,
                          to_lower = TRUE)
  deduped <- extract_unique_references(my_df, matches = dups)
  deduped2 <- deduplicate(my_df, "title",
                          rm_punctuation = TRUE,
                          to_lower = TRUE)

  expect_equal(length(dups), nrow(my_df))
  expect_true(all(dups[5:6] == dups[1:2]))
  expect_equal(length(unique(dups)), nrow(deduped))
  expect_equal(deduped, deduped2)
})


