my_df <-
  data.frame(
    title = c(
      "Morphological diversity and phenotypic plasticity in the threatened
      British white‐clawed crayfish (Austropotamobius pallipes)",

      "Predatory functional response and prey choice identify predation
      differences between native/invasive and parasitised/unparasitised crayfish",

      "Effect of pH on growth and survival in the freshwater
      crayfish Austropotamobius pallipes",

      "Morphological diversity and phenotypic plasticity in the threatened
      British white clawed crayfish austropotamobius pallipes"
    ),
    year=c("2012", "2012", "2013", "2012"))


dups <- find_duplicates(my_df, match_variable = "title", match_function = "stringdist")

deduplicate(my_df, matches = dups)
