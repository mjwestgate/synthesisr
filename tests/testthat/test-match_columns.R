my_df <- data.frame(PY = 2019,
                    DB = "Scopus",
                    TI = "revtools: An R package to support article screening for evidence synthesis",
                    AU = "Westgate, M.J.")

expect(all(
  colnames(match_columns(my_df)) == c("database", "author", "year", "title")
),
"Column names not being matched in proper order to code lookup")


my_df2 <- data.frame(
  PY = 2019,
  doi = "10.1186/s13750-019-0167-1",
  DB = "Scopus",
  TI = "revtools: An R package to support article screening for evidence synthesis",
  AU = "Westgate, M.J.",
  random_noise = c("non-bibliographic data")
)

expect(all(
  colnames(match_columns(my_df2)) == c("database", "author", "year", "title", "doi", "random_noise")
),
"Unidentified columns not being appended to data.frame")
