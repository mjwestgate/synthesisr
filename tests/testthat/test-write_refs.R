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

evi_ris <- write_refs(df, format = "ris")
evi_bib <- write_refs(df, format = "bib")


expect(
  detect_parser(evi_bib) == "parse_bibtex",
  "Bibtex formatted references not exporting as expected"
)

# note that this does not necessarily mean external programs will be able to read the file
expect(
  detect_parser(evi_ris) == "parse_ris",
  "RIS formatted references not exporting as expected"
)
expect(
  any(grep("ER ", evi_ris, ignore.case = FALSE)),
  "ER tags for end of record not being included in RIS exports which causes errors with some external bibliographic programs"
)
