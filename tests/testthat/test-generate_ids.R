expect(all(generate_ids(1:10)==c("ref_01", "ref_02", "ref_03", "ref_04", "ref_05", "ref_06",
                             "ref_07", "ref_08", "ref_09", "ref_10")), "Default reference IDs not being generated as expected")

# I will write a more thorough test once I sort out why parse_pubmed is not retrieving years
# We should probably be able to fix that problem pretty quickly by removing letters with gsub and then finding numbers of length 4
