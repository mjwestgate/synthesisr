my_df <- data.frame(
  PY = 2019,
  doi = "10.1186/s13750-019-0167-1",
  DB = "Scopus",
  TI = "revtools: An R package to support article screening for evidence synthesis",
  AU = "Westgate, M.J.",
  random_noise = c("non-bibliographic data")
)

expect(!any(lapply(remove_factors(my_df), class) %in% "factor"),
       "Factors not removed from data.frame")
