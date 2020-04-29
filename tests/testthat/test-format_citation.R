file <- c(
  "TY  - JOUR",
  "AU  - Haddaway, Neal R.",
  "AU  - Feierman, Andrew",
  "AU  - Grainger, Matthew J.",
  "AU  - Gray, Charles T.",
  "AU  - Tanriver-Ayder, Ezgi",
  "AU  - Dhaubanjar, Sanita",
  "AU  - Westgate, Martin J.",
  "PY  - 2019",
  "DA  - 2019/06/04",
  "TI  - EviAtlas: a tool for visualising evidence synthesis databases",
  "JO  - Environmental Evidence",
  "SP  - 22",
  "VL  - 8",
  "IS  - 1",
  "SN  - 2047-2382",
  "UR  - https://doi.org/10.1186/s13750-019-0167-1",
  "DO  - 10.1186/s13750-019-0167-1",
  "ID  - Haddaway2019",
  "ER  - "
)

tmp <- tempfile()

writeLines(file, tmp)

df <- read_ref(tmp, return_df = TRUE, verbose = TRUE)

expect(format_citation(df[1,])=="Haddaway, Neal R. et al. (2019) Eviatlas: a Tool for Visualising Evidence Synthesis Databases. Environmental Evidence.",
       "Output citation does not match expected format for data.frame")

bib <- as.bibliography(df)

expect(all(as.character(format_citation(bib))==as.character(format_citation(df))), "Citations are not being formatted the same for bibliographies and data.frames")

