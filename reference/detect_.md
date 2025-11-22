# Detect file formatting information

Bibliographic data can be stored in a number of different file types,
meaning that detecting consistent attributes of those files is necessary
if they are to be parsed accurately. These functions attempt to identify
some of those key file attributes. Specifically, `detect_parser()`
determines which
[parse\_](https://martinwestgate.com/synthesisr/reference/parse_.md)
function to use; `detect_delimiter()` and `detect_lookup()` identify
different attributes of RIS files; and `detect_year()` attempts to fill
gaps in publication years from other information stored in a `tibble`.

## Usage

``` r
detect_parser(x)

detect_delimiter(x)

detect_lookup(tags)

detect_year(df)
```

## Arguments

- x:

  A character vector containing bibliographic data

- tags:

  A character vector containing RIS tags.

- df:

  a data.frame containing bibliographic data

## Value

`detect_parser()` and `detect_delimiter()` return a length-1 character;
`detect_year()` returns a character vector listing estimated publication
years; and `detect_lookup()` returns a `data.frame.`

## Examples

``` r
revtools <- c(
  "",
  "PMID- 31355546",
  "VI  - 10",
  "IP  - 4",
  "DP  - 2019 Dec",
  "TI  - revtools: An R package to support article
         screening for evidence synthesis.",
  "PG  - 606-614",
  "LID - 10.1002/jrsm.1374 [doi]",
  "AU  - Westgate MJ",
  "LA  - eng",
  "PT  - Journal Article",
  "JT  - Research Synthesis Methods",
  ""
)

# detect basic attributes of ris files
detect_parser(revtools)
#> [1] "parse_pubmed"
detect_delimiter(revtools)
#> [1] "space"

# determine which tag format to use
tags <- trimws(unlist(lapply(
  strsplit(revtools, "- "),
  function(a){a[1]}
)))
pubmed_tag_list <- detect_lookup(tags[!is.na(tags)])

# find year data in other columns
df <- as.data.frame(parse_pubmed(revtools))
df$year <- detect_year(df)
```
