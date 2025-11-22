# Add line breaks to one or more strings

This function takes a vector of strings and adds line breaks every n
characters. Primarily built to be called internally by
[`format_citation()`](https://martinwestgate.com/synthesisr/reference/format_citation.md),
this function has been made available as it can be useful in other
contexts.

## Usage

``` r
add_line_breaks(x, n = 50, max_n = NULL, html = FALSE, max_time = NULL)
```

## Arguments

- x:

  Either a string or a vector; if the vector is not of class character
  if will be coerced to one using
  [`as.character()`](https://rdrr.io/r/base/character.html).

- n:

  Numeric: The desired number of characters that should separate
  consecutive line breaks.

- max_n:

  DEPRECATED: If provided will currently overwrite `n`; otherwise
  synonymous with `n` and will be removed from future versions.

- html:

  Logical: Should the line breaks be specified in html?

- max_time:

  DEPRECATED: Previously the maximum amount of time (in seconds) allowed
  to adjust groups until character thresholds are reached. Ignored.

## Value

Returns the input vector unaltered except for the addition of line
breaks.

## Details

Line breaks are only added between words, so the value of n is actually
a threshold value rather than being matched exactly.

## Examples

``` r
add_line_breaks(c("On the Origin of Species"), n = 10)
#> [1] "On the\nOrigin\nof\nSpecies"
```
