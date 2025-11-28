# Remove duplicates from a bibliographic data set

Removes duplicates using sensible defaults

## Usage

``` r
deduplicate(data, match_by, method, type = "merge", ...)
```

## Arguments

- data:

  A `tibble` containing bibliographic information.

- match_by:

  Name of the (single) column in `data` where duplicates should be
  sought.

- method:

  The duplicate detection function to use; see
  [`string_`](https://martinwestgate.com/synthesisr/reference/string_.md)
  or [`fuzz_`](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
  for examples. Passed to
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

A `tibble` containing data identified as unique.

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
my_df <-  tibble::tibble(
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
              "Grames et al", "Pick et al", NA, NA))

# run deduplication
dups <- find_duplicates(
  my_df$title,
  method = "string_osa",
  rm_punctuation = TRUE,
  to_lower = TRUE)

extract_unique_references(my_df, matches = dups)
#> # A tibble: 4 × 4
#>   title                                               year  authors n_duplicates
#>   <chr>                                               <chr> <chr>          <dbl>
#> 1 EviAtlas: a tool for visualising evidence synthesi… 2019  Haddaw…            2
#> 2 revtools: An R package to support article screenin… 2019  Westga…            2
#> 3 An automated approach to identifying search terms … 2019  Grames…            1
#> 4 Reproducible, flexible and high-throughput data ex… 2019  Pick e…            1

# or, in one line:
deduplicate(my_df, "title",
  method = "string_osa",
  rm_punctuation = TRUE,
  to_lower = TRUE)
#> # A tibble: 4 × 4
#>   title                                               year  authors n_duplicates
#>   <chr>                                               <chr> <chr>          <dbl>
#> 1 EviAtlas: a tool for visualising evidence synthesi… 2019  Haddaw…            2
#> 2 revtools: An R package to support article screenin… 2019  Westga…            2
#> 3 An automated approach to identifying search terms … 2019  Grames…            1
#> 4 Reproducible, flexible and high-throughput data ex… 2019  Pick e…            1
```
