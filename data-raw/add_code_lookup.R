code_lookup <- readr::read_csv("./data-raw/code_lookup.csv")
usethis::use_data(code_lookup, internal = TRUE)
