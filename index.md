![](reference/figures/logo.png)

## Tools for bibliographic data

Metascientific analyses - such as systematic reviews and meta-analyses -
commonly involve searches of multiple bibliographic databases. These
databases use a range of different data formats, and have differing
degrees of overlap in the journals and articles that they index. To
streamline the process of importing, assembling, and deduplicating
results, `synthesisr` recognizes the file output of commonly used
databases for systematic reviews and merges results into a tibble.

If you have questions, comments, feature requests, or find a bug,
[please open an issue](https://github.com/mjwestgate/synthesisr/issues).

## Installation

`synthesisr` is available on CRAN:

``` r
install.packages("synthesisr")
```

Alternatively you can install from GitHub:

``` r
remotes::install_github("mjwestgate/synthesisr")
```

## Basic usage

The default function for importing bibliographic data is
[`read_refs()`](https://martinwestgate.com/synthesisr/reference/read_refs.md):

``` r
x <- read_refs("a_file.bib")
```
