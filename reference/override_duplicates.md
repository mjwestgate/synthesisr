# Manually override duplicates

Re-assign group numbers to text that was classified as duplicated but is
unique.

## Usage

``` r
override_duplicates(matches, overrides)
```

## Arguments

- matches:

  Numeric: a vector of group numbers for texts that indicates duplicates
  and unique values returned by the
  [`find_duplicates`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md)
  function.

- overrides:

  Numeric: a vector of group numbers that are not true duplicates.

## Value

The input `matches` vector with unique group numbers for members of
groups that the user overrides.
