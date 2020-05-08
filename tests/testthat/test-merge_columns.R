my_df <- data.frame(PY = 2019,
                    DB = "Scopus",
                    TI = "revtools: An R package to support article screening for evidence synthesis",
                    AU = "Westgate, M.J.")

my_df2 <- data.frame(
  PY = 2019,
  doi = "10.1186/s13750-019-0167-1",
  DB = "Scopus",
  TI = "revtools: An R package to support article screening for evidence synthesis",
  AU = "Westgate, M.J.",
  random_noise = c("non-bibliographic data")
)

db <- merge_columns(list(my_df, my_df2))
vars <- unique(c(colnames(my_df), colnames(my_df2)))

expect(
  all(vars %in% colnames(db)),
  "Not all columns from all data frames were merged into the new data.frame"
)
