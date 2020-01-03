my_df <-
  data.frame(
    title = c(
      "Morphological diversity and phenotypic plasticity in the threatened
      British white‐clawed crayfish (Austropotamobius pallipes)",

      "Predatory functional response and prey choice identify predation
      differences between native/invasive and parasitised/unparasitised crayfish",

      "Effect of pH on growth and survival in the freshwater
      crayfish Austropotamobius pallipes"
      ),
    year=c("2012", "2012", "2013"))

terms <- c("crayfish", "diversity", "plasticity",
           "predation", "survival")

create_dfm(my_df$title, features = terms, closure = "none")
