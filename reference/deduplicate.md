# Remove duplicates from a bibliographic data set

Removes duplicates using sensible defaults

## Usage

``` r
deduplicate(data, match_by, method, type = "merge", ...)
```

## Arguments

- data:

  A `data.frame` containing bibliographic information.

- match_by:

  Name of the column in `data` where duplicates should be sought.

- method:

  The duplicate detection function to use; see `link{string_}` or
  `link{fuzz_}` for examples. Passed to
  [`find_duplicates()`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md).

- type:

  How should entries be selected? Default is `"merge"` which selects the
  entries with the largest number of characters in each column.
  Alternatively `"select"` returns the row with the highest total number
  of characters.

- ...:

  Arguments passed to
  [`find_duplicates()`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md).

## Value

A `data.frame` containing data identified as unique.

## Details

This is a wrapper function to
[`find_duplicates()`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md)
and
[`extract_unique_references()`](https://martinwestgate.com/synthesisr/reference/extract_unique_references.md),
which tries to choose some sensible defaults. Use with care.

## See also

[`find_duplicates()`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md)
and
[`extract_unique_references()`](https://martinwestgate.com/synthesisr/reference/extract_unique_references.md)
for underlying functions.

## Examples

``` r
my_df <-  data.frame(
  title = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",
    "revtools: An R package to support article screening for evidence synthesis",
    "An automated approach to identifying search terms for systematic reviews",
    "Reproducible, flexible and high-throughput data extraction from primary literature",
    "eviatlas:tool for visualizing evidence synthesis databases.",
    "REVTOOLS a package to support article-screening for evidence synthsis"
  ),
  year = c("2019", "2019", "2019", "2019", NA, NA),
  authors = c("Haddaway et al", "Westgate",
              "Grames et al", "Pick et al", NA, NA),
  stringsAsFactors = FALSE
)

# run deduplication
dups <- find_duplicates(
  my_df$title,
  method = "string_osa",
  rm_punctuation = TRUE,
  to_lower = TRUE
)

extract_unique_references(my_df, matches = dups)
#>                                                                                title
#> 1                      EviAtlas: a tool for visualising evidence synthesis databases
#> 2         revtools: An R package to support article screening for evidence synthesis
#> 3           An automated approach to identifying search terms for systematic reviews
#> 4 Reproducible, flexible and high-throughput data extraction from primary literature
#>   year        authors n_duplicates
#> 1 2019 Haddaway et al            2
#> 2 2019       Westgate            2
#> 3 2019   Grames et al            1
#> 4 2019     Pick et al            1

# or, in one line:
deduplicate(my_df, "title",
  method = "string_osa",
  rm_punctuation = TRUE,
  to_lower = TRUE)
#>                                                                                title
#> 1                      EviAtlas: a tool for visualising evidence synthesis databases
#> 2         revtools: An R package to support article screening for evidence synthesis
#> 3           An automated approach to identifying search terms for systematic reviews
#> 4 Reproducible, flexible and high-throughput data extraction from primary literature
#>   year        authors n_duplicates
#> 1 2019 Haddaway et al            2
#> 2 2019       Westgate            2
#> 3 2019   Grames et al            1
#> 4 2019     Pick et al            1
```
