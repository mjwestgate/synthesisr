my_df <-  data.frame(
  title = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",

    "revtools: An R package to support article screening for evidence synthesis",

    "An automated approach to identifying search terms for systematic reviews
    using keyword co‐occurrence networks",

    "Reproducible, flexible and high‐throughput data extraction from primary literature:
    The metaDigitise r package",

    "eviatlas:tool for visualizing evidence synthesis databases.",

    "REVTOOLS a package to support article-screening for evidence synthsis"
  ),

  year = c("2019", "2019", "2019", "2019", NA, NA),

  authors = c("Haddaway et al", "Westgate",
              "Grames et al", "Pick et al", NA, NA)
)

# run deduplication
dups <- find_duplicates(
  my_df$title,
  match_function = "stringdist",
  rm_punctuation = TRUE,
  to_lower = TRUE
)

extract_unique_references(my_df, matches = dups)

# or, in one line:
deduplicate(my_df, "title")