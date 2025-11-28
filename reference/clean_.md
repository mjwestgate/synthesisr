# Clean a `tibble` or vector

Cleans column and author names

## Usage

``` r
clean_df(x)

clean_authors(x)

clean_colnames(x)
```

## Arguments

- x:

  A `tibble` with bibliographic information.

## Value

Returns the input, but cleaner.

## Examples

``` r
df <-  data.frame(
  X..title. = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",
    "revtools: An R package to support article screening for evidence synthesis",
    "An automated approach to identifying search terms for systematic reviews",
    "Reproducible, flexible and high-throughput data extraction from primary literature"),
  YEAR = c("2019", "2019", "2019", "2019"),
  authors = c(
    "Haddaway et al",
    "Westgate",
    "EM Grames AND AN Stillman  & MW Tingley and CS Elphick",
    "Pick et al"))
clean_df(df)
#>                                                                                title
#> 1                      EviAtlas: a tool for visualising evidence synthesis databases
#> 2         revtools: An R package to support article screening for evidence synthesis
#> 3           An automated approach to identifying search terms for systematic reviews
#> 4 Reproducible, flexible and high-throughput data extraction from primary literature
#>   year                                                  author
#> 1 2019                                          Haddaway et al
#> 2 2019                                                Westgate
#> 3 2019 EM Grames and AN Stillman and MW Tingley and CS Elphick
#> 4 2019                                              Pick et al

# or use sub-functions
clean_colnames(df)
#>                                                                                title
#> 1                      EviAtlas: a tool for visualising evidence synthesis databases
#> 2         revtools: An R package to support article screening for evidence synthesis
#> 3           An automated approach to identifying search terms for systematic reviews
#> 4 Reproducible, flexible and high-throughput data extraction from primary literature
#>   year                                                 author
#> 1 2019                                         Haddaway et al
#> 2 2019                                               Westgate
#> 3 2019 EM Grames AND AN Stillman  & MW Tingley and CS Elphick
#> 4 2019                                             Pick et al
clean_authors(df)
#>                                                                            X..title.
#> 1                      EviAtlas: a tool for visualising evidence synthesis databases
#> 2         revtools: An R package to support article screening for evidence synthesis
#> 3           An automated approach to identifying search terms for systematic reviews
#> 4 Reproducible, flexible and high-throughput data extraction from primary literature
#>   YEAR                                                authors
#> 1 2019                                         Haddaway et al
#> 2 2019                                               Westgate
#> 3 2019 EM Grames AND AN Stillman  & MW Tingley and CS Elphick
#> 4 2019                                             Pick et al
```
