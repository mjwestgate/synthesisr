# Package index

## Overview

- [`synthesisr`](https://martinwestgate.com/synthesisr/reference/synthesisr-package.md)
  [`synthesisr-package`](https://martinwestgate.com/synthesisr/reference/synthesisr-package.md)
  : synthesisr: Import, assemble, and deduplicate bibiliographic
  datasets

## Import & Export

- [`read_refs()`](https://martinwestgate.com/synthesisr/reference/read_refs.md)
  : Import bibliographic data files

- [`write_refs()`](https://martinwestgate.com/synthesisr/reference/write_refs.md)
  [`write_bib()`](https://martinwestgate.com/synthesisr/reference/write_refs.md)
  [`write_ris()`](https://martinwestgate.com/synthesisr/reference/write_refs.md)
  : Export data to a bibliographic format

- [`detect_parser()`](https://martinwestgate.com/synthesisr/reference/detect_.md)
  [`detect_delimiter()`](https://martinwestgate.com/synthesisr/reference/detect_.md)
  [`detect_lookup()`](https://martinwestgate.com/synthesisr/reference/detect_.md)
  [`detect_year()`](https://martinwestgate.com/synthesisr/reference/detect_.md)
  : Detect file formatting information

- [`parse_bibtex()`](https://martinwestgate.com/synthesisr/reference/parse_.md)
  [`parse_pubmed()`](https://martinwestgate.com/synthesisr/reference/parse_.md)
  [`parse_ris()`](https://martinwestgate.com/synthesisr/reference/parse_.md)
  : Parse bibliographic text in a variety of formats

- [`clean_df()`](https://martinwestgate.com/synthesisr/reference/clean_.md)
  [`clean_authors()`](https://martinwestgate.com/synthesisr/reference/clean_.md)
  [`clean_colnames()`](https://martinwestgate.com/synthesisr/reference/clean_.md)
  :

  Clean a `tibble` or vector

- [`code_lookup`](https://martinwestgate.com/synthesisr/reference/code_lookup.md)
  : Bibliographic code lookup for search results assembly

## Formatting

- [`summary(`*`<bibliography>`*`)`](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  [`print(`*`<bibliography>`*`)`](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  [`` `[`( ``*`<bibliography>`*`)`](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  [`c(`*`<bibliography>`*`)`](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  [`as.data.frame(`*`<bibliography>`*`)`](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  [`as.bibliography()`](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  [`as_tibble(`*`<bibliography>`*`)`](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  : bibliography-class
- [`format_citation()`](https://martinwestgate.com/synthesisr/reference/format_citation.md)
  : Format a citation
- [`add_line_breaks()`](https://martinwestgate.com/synthesisr/reference/add_line_breaks.md)
  : Add line breaks to one or more strings

## Deduplication

- [`deduplicate()`](https://martinwestgate.com/synthesisr/reference/deduplicate.md)
  : Remove duplicates from a bibliographic data set
- [`find_duplicates()`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md)
  : Detect duplicate values
- [`extract_unique_references()`](https://martinwestgate.com/synthesisr/reference/extract_unique_references.md)
  : Remove duplicates from a bibliographic data set
- [`review_duplicates()`](https://martinwestgate.com/synthesisr/reference/review_duplicates.md)
  : Manually review potential duplicates
- [`override_duplicates()`](https://martinwestgate.com/synthesisr/reference/override_duplicates.md)
  : Manually override duplicates
- [`fuzzdist()`](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
  [`fuzz_m_ratio()`](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
  [`fuzz_partial_ratio()`](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
  [`fuzz_token_sort_ratio()`](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
  [`fuzz_token_set_ratio()`](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
  : Calculate similarity between two strings
- [`string_osa()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_lv()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_dl()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_hamming()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_lcs()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_qgram()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_cosine()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_jaccard()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_jw()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  [`string_soundex()`](https://martinwestgate.com/synthesisr/reference/string_.md)
  : Calculate similarity between two strings
