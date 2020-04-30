author_list <-
  c(
    "Haddaway, N.R., A. Feirman, M.J. Grainger, C.T. Gray, E. Tanriver-Ayder, S. Dhaubanjar, & M.J Westgate",
    "Grames, E.M., A.N. Stillman, M.W. Tingley AND C.S. Elphick"
  )

expect(!any(grepl(
  pattern = c("&"), clean_authors(author_list)
)), "Ampersands not being removed from author list")

expect(!any(grepl(
  pattern = c("AND"), clean_authors(author_list)
)), "Capital AND not being removed from author list")
