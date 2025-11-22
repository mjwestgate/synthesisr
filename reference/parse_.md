# Parse bibliographic text in a variety of formats

Text in standard formats - such as imported via
[`base::readLines()`](https://rdrr.io/r/base/readLines.html) - can be
parsed using a variety of standard formats. Use
[`detect_parser()`](https://martinwestgate.com/synthesisr/reference/detect_.md)
to determine which is the most appropriate parser for your situation.
Note that `parse_tsv()` and `parse_csv()` are maintained for backwards
compatability only; within `read_ref` these have been replaced by
[`vroom::vroom()`](https://vroom.r-lib.org/reference/vroom.html).

## Usage

``` r
parse_bibtex(x)

parse_csv(x)

parse_tsv(x)

parse_pubmed(x)

parse_ris(x, tag_naming = "best_guess")
```

## Arguments

- x:

  A character vector containing bibliographic information in ris format.

- tag_naming:

  What format are ris tags in? Defaults to `"best_guess"` See
  [`read_refs()`](https://martinwestgate.com/synthesisr/reference/read_refs.md)
  for a list of accepted arguments.

## Value

Returns an object of class `bibliography` (ris, bib, or pubmed formats)
or `data.frame` (csv or tsv).

## Examples

``` r
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
#> [1] "parse_ris"
df <- as.data.frame(parse_ris(eviatlas))
ris_out <- write_refs(df, format = "ris", file = FALSE)
#> Error in check_filename(file): argument 'file' should be an object of class `character`
```
