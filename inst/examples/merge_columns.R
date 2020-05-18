df_1 <-  data.frame(
  title = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",
    "revtools: An R package to support article screening for evidence synthesis"
  ),
  year = c("2019", "2019")
)

df_2 <-  data.frame(
  title = c(
    "An automated approach to identifying search terms for systematic reviews",
    "Reproducible, flexible and high-throughput data extraction from primary literature"
  ),
  authors = c("Grames et al", "Pick et al")
)

merge_columns(df_1, df_2)
