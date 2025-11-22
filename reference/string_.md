# Calculate similarity between two strings

These functions each access a specific `"methods"` argument provided by
`stringdist`, and are provided for convenient calling by
[`find_duplicates()`](https://martinwestgate.com/synthesisr/reference/find_duplicates.md).
They do not include any new functionality beyond that given by
`stringdist`, which you should use for your own analyses.

## Usage

``` r
string_osa(a, b)

string_lv(a, b)

string_dl(a, b)

string_hamming(a, b)

string_lcs(a, b)

string_qgram(a, b)

string_cosine(a, b)

string_jaccard(a, b)

string_jw(a, b)

string_soundex(a, b)
```

## Arguments

- a:

  A character vector of items to match to b.

- b:

  A character vector of items to match to a.

## Value

Returns a score of same length as b, giving the dissimilarity between a
and b.
