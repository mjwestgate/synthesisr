# Manually review potential duplicates

Allows users to manually review articles classified as duplicates.

## Usage

``` r
review_duplicates(text, matches)
```

## Arguments

- text:

  A character vector of the text that was used to identify potential
  duplicates.

- matches:

  Numeric: a vector of group numbers for texts that indicates duplicates
  and unique values returned by the
  [`find_duplicates`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md)
  function.

## Value

A `data.frame` of potential duplicates grouped together.
