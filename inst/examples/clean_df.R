my_df <-  data.frame(
  X..title. = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",

    "revtools: An R package to support article screening for evidence synthesis",

    "An automated approach to identifying search terms for
      systematic reviews using keyword co‐occurrence networks",

    "Reproducible, flexible and high‐throughput data extraction
    from primary literature: The metaDigitise r package"),

  YEAR = c("2019", "2019", "2019", "2019"),

  authors = c("Haddaway et al", "Westgate",
              "Grames et al", "Pick et al"))

clean_df(my_df)
