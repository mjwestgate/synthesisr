# Remove duplicates from a bibliographic data set

Given a list of duplicate entries and a data set, this function extracts
only unique references.

## Usage

``` r
extract_unique_references(data, matches, type = "merge")
```

## Arguments

- data:

  A `data.frame` containing bibliographic information.

- matches:

  A vector showing which entries in `data` are duplicates.

- type:

  How should entries be selected to retain? Default is `"merge"`, which
  selects the entries with the largest number of characters in each
  column. Alternatively, `"select"` returns the row with the highest
  total number of characters.

## Value

Returns a `data.frame` of unique references.

## See also

[`find_duplicates()`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md),
[`deduplicate()`](https://martinwestgate.com/synthesisr/reference/deduplicate.md)

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
