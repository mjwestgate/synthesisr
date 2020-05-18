df <-  data.frame(
  X..title. = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",
    "revtools: An R package to support article screening for evidence synthesis",
    "An automated approach to identifying search terms for systematic reviews",
    "Reproducible, flexible and high-throughput data extraction from primary literature"),
  YEAR = c("2019", "2019", "2019", "2019"),
  authors = c(
    "Haddaway et al",
    "Westgate",
    "EM Grames AND AN Stillman  & MW Tingley and CS Elphick",
    "Pick et al")
)

clean_df(df)

# or use sub-functions
colnames(df) <- clean_colnames(df)
# colnames(df) <- clean_colnames(colnames(df)) # also works
df$author <- clean_authors(df$author)

