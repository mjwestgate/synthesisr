# Detect duplicate values

Identifies duplicate bibliographic entries using different duplicate
detection methods.

## Usage

``` r
find_duplicates(
  data,
  method = "exact",
  group_by,
  threshold,
  to_lower = FALSE,
  rm_punctuation = FALSE
)
```

## Arguments

- data:

  A character vector containing duplicate bibliographic entries.

- method:

  A string indicating how matching should be calculated. Either
  `"exact"` for exact matching (the default), or the name of a function
  for calculating string distance.

- group_by:

  An optional vector, data.frame or list containing data to use as
  'grouping' variables; that is, categories within which duplicates
  should be sought. Defaults to NULL, in which case all entries are
  compared against all others. Ignored if `method = "exact"`.

- threshold:

  Numeric: the cutoff threshold for deciding if two strings are
  duplicates. Sensible values depend on the `method` chosen. Defaults to
  5 if `method = "string_osa"` and must be specified in all other
  instances except `method = "exact"` (where no threshold is required).

- to_lower:

  Logical: Should all entries be converted to lower case before
  calculating string distance? Defaults to `FALSE.`

- rm_punctuation:

  Logical: Should punctuation should be removed before calculating
  string distance? Defaults to `FALSE.`

## Value

Returns a vector of same length as `nrow(data)`, where duplicated values
have the same integer, and `attributes` listing methods used.

## See also

[`string_`](https://martinwestgate.com/synthesisr/reference/string_.md)
or [`fuzz_`](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
for suitable functions to pass to `methods`;
[`extract_unique_references()`](https://martinwestgate.com/synthesisr/reference/extract_unique_references.md)
and
[`deduplicate()`](https://martinwestgate.com/synthesisr/reference/deduplicate.md)
for higher-level functions.

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
