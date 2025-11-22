# Calculate similarity between two strings

These functions duplicate the approach of the 'fuzzywuzzy' Python
library for calculating string similarity.

## Usage

``` r
fuzzdist(
  a,
  b,
  method = c("fuzz_m_ratio", "fuzz_partial_ratio", "fuzz_token_sort_ratio",
    "fuzz_token_set_ratio")
)

fuzz_m_ratio(a, b)

fuzz_partial_ratio(a, b)

fuzz_token_sort_ratio(a, b)

fuzz_token_set_ratio(a, b)
```

## Arguments

- a:

  A character vector of items to match to b.

- b:

  A character vector of items to match to a.

- method:

  The method to use for fuzzy matching.

## Value

Returns a score of same length as b, giving the proportional
dissimilarity between a and b.

## Note

`fuzz_m_ratio()` is a measure of the number of letters that match
between two strings. It is calculated as one minus two times the number
of matched characters, divided by the number of characters in both
strings.

`fuzz_partial_ratio()` calculates the extent to which one string is a
subset of the other. If one string is a perfect subset, then this will
be zero.

`fuzz_token_sort_ratio()` sorts the words in both strings into
alphabetical order, and checks their similarity using `fuzz_m_ratio()`.

`fuzz_token_set_ratio()` is similar to `fuzz_token_sort_ratio()`, but
compares both sorted strings to each other, and to a third group made of
words common to both strings. It then returns the maximum value of
`fuzz_m_ratio()` from these comparisons.

`fuzzdist()` is a wrapper function, for compatability with `stringdist`.

## Examples

``` r
fuzzdist("On the Origin of Species",
         "Of the Original Specs",
         method = "fuzz_m_ratio")
#> [1] 0.4666667
```
