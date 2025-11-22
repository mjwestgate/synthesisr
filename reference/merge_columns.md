# Bind two or more data frames with different columns

Takes two or more `data.frames` with different column names or different
column orders and binds them to a single `data.frame.` This function is
maintained for backwards compatibility, but it is synonymous with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
and will be depracated in future.

## Usage

``` r
merge_columns(x, y)
```

## Arguments

- x:

  Either a data.frame or a list of data.frames.

- y:

  A data.frame, optional if x is a list.

## Value

Returns a single data.frame with all the input data frames merged.

## Examples

``` r
df_1 <-  data.frame(
  title = c(
    "EviAtlas: a tool for visualising evidence synthesis databases",
    "revtools: An R package to support article screening for evidence synthesis"
  ),
  year = c("2019", "2019")
)

df_2 <-  data.frame(
  title = c(
    "An automated approach to identifying search terms for systematic reviews",
    "Reproducible, flexible and high-throughput data extraction from primary literature"
  ),
  authors = c("Grames et al", "Pick et al")
)

merge_columns(df_1, df_2)
#>                                                                                title
#> 1                      EviAtlas: a tool for visualising evidence synthesis databases
#> 2         revtools: An R package to support article screening for evidence synthesis
#> 3           An automated approach to identifying search terms for systematic reviews
#> 4 Reproducible, flexible and high-throughput data extraction from primary literature
#>   year      authors
#> 1 2019         <NA>
#> 2 2019         <NA>
#> 3 <NA> Grames et al
#> 4 <NA>   Pick et al
```
