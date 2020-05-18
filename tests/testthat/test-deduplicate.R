my_df <-  data.frame(
  title = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",

    "revtools: An R package to support article screening for evidence synthesis",

    "An automated approach to identifying search terms for systematic reviews
    using keyword co-occurrence networks",

    "Reproducible, flexible and high-throughput data extraction from primary literature:
    The metaDigitise r package",

    "eviatlas:tool for visualizing evidence synthesis databases.",

    "REVTOOLS a package to support article-screening for evidence synthsis"
  ),

  year = c("2019", "2019", "2019", "2019", NA, NA),

  authors = c("Haddaway et al", "Westgate",
              "Grames et al", "Pick et al", NA, NA),
  stringsAsFactors = FALSE
)

# run deduplication
dups <- find_duplicates(
  my_df$title,
  method = "string_osa",
  rm_punctuation = TRUE,
  to_lower = TRUE
)

expect(
  length(dups) == nrow(my_df),
  "Not all rows in df have been classified as duplicates or unique entries"
)

expect(
  all(dups[5:6] == dups[1:2]),
  "Not detecting duplicated titles in example df"
)

deduped <- extract_unique_references(my_df, matches = dups)

expect(
  length(unique(dups)) == nrow(deduped),
  "Not all duplicate entries ignored when extracting unique references"
)

deduped2 <- deduplicate(my_df, "title",
  rm_punctuation = TRUE,
  to_lower = TRUE)

expect(
  all.equal(deduped, deduped2),
  "deduplicate not returning the same result as combining find_duplicates and extract_unique_references"
)
