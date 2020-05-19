eviatlas <- c(
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

detect_parser(eviatlas) # = "parse_ris"
df <- as.data.frame(parse_ris(eviatlas))
ris_out <- write_refs(df, format = "ris", file = FALSE)
