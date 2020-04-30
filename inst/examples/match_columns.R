perspective <- c(
 "TY - JOUR",
 "AU - Nakagawa, Shinichi",
 "AU - Dunn, Adam G.", "AU - Lagisz, Malgorzata",
 "AU - Bannach-Brown, Alexandra", "AU - Grames, Eliza M.",
 "AU - Sánchez-Tójar, Alfredo", "AU - O’Dea, Rose E.",
 "AU - Noble, Daniel W. A.", "AU - Westgate, Martin J.",
 "AU - Arnold, Pieter A.", "AU - Barrow, Stuart",
 "AU - Bethel, Alison", "AU - Cooper, Eve",
 "AU - Foo, Yong Zhi", "AU - Geange, Sonya R.",
 "AU - Hennessy, Emily", "AU - Mapanga, Witness",
 "AU - Mengersen, Kerrie", "AU - Munera, Claudia",
 "AU - Page, Matthew J.", "AU - Welch, Vivian",
 "AU - Carter, Matthew", "AU - Forbes, Owen",
 "AU - Furuya-Kanamori, Luis", "AU - Gray, Charles T.",
 "AU - Hamilton, W. Kyle", "AU - Kar, Fonti",
 "AU - Kothe, Emily", "AU - Kwong, Joey",
 "AU - McGuinness, Luke A.", "AU - Martin, Paige",
 "AU - Ngwenya, Mandlenkosi", "AU - Penkin, Christopher",
 "AU - Perez, Daniel", "AU - Schermann, Michael",
 "AU - Senior, Alistair M.", "AU - Vásquez, Juan",
 "AU - Viechtbauer, Wolfgang", "AU - White, Thomas E.",
 "AU - Whitelaw, Mitchell", "AU - Haddaway, Neal R.",
 "AU - Evidence Synthesis Hackathon 2019 Participants",
 "PY - 2020",
 "DA - 2020/04/01",
 "TI - A new ecosystem for evidence synthesis",
 "JO - Nature Ecology & Evolution",
 "SP - 498",
 "EP - 501",
 "VL - 4",
 "IS - 4",
 "SN - 2397-334X",
 "UR - https://doi.org/10.1038/s41559-020-1153-2",
 "DO - 10.1038/s41559-020-1153-2",
 "ID - Nakagawa2020",
 "ER - "

)
df <- as.data.frame(parse_ris(revtools))
colnames(df) # some parsed, some not

formatted <- match_columns(df)
colnames(formatted) # columns matched to reference dataset
