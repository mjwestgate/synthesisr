revtools <- c(
  "",
  "PMID- 31355546",
  "VI  - 10",
  "IP  - 4",
  "DP  - 2019 Dec",
  "TI  - revtools: An R package to support article
         screening for evidence synthesis.",
  "PG  - 606-614",
  "LID - 10.1002/jrsm.1374 [doi]",
  "AU  - Westgate MJ",
  "LA  - eng",
  "PT  - Journal Article",
  "JT  - Research Synthesis Methods",
  ""
)

# detect basic attributes of ris files
detect_parser(revtools)
detect_delimiter(revtools)

# determine which tag format to use
tags <- trimws(unlist(lapply(
  strsplit(revtools, "- "),
  function(a){a[1]}
)))
pubmed_tag_list <- detect_lookup(tags[!is.na(tags)])

# find year data in other columns
df <- as.data.frame(parse_pubmed(revtools))
df$year <- detect_year(df)
