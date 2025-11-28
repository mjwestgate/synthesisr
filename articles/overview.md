# Overview

## Introduction

Systematic review searches include multiple databases that export
results in a variety of formats with overlap in coverage between
databases. To streamline the process of importing, assembling, and
deduplicating results, `synthesisr` recognizes bibliographic files
exported from databases commonly used for systematic reviews and merges
results into a standardized format.

We begin, as always, by loading the package:

``` r
# install.packages("synthesisr") # if needed
library(synthesisr)
```

    #> ℹ Loading synthesisr

## Read and assemble bibliographic files

`synthesisr` can read any BibTex or RIS formatted bibliographic data
files. It detects whether files are more bib-like or ris-like and
imports them accordingly. Note that files from some databases may
contain non-standard fields or non-standard characters that cause import
failure in rare cases; if this happens, we recommend converting the file
in open source bibliographic management software such as Zotero.

In the code below, we will demonstrate how to read and assemble
bibliographic data files with example datasets included in the
`synthesisr` package. Note that if you are using the code with your own
data, you will not need to use
[`system.file()`](https://rdrr.io/r/base/system.file.html) and instead
will want to pass a character vector of the path(s) to the file(s) you
want to import. For example, if you have saved all your search results
in a directory called “search_results”, you may want to use
`list.files("./search_results/")` instead.

``` r
# system.file will look for the path to where synthesisr is installed
# by using the example bibliographic data files, you can reproduce the vignette
bibfiles <- list.files(
  system.file("extdata/", package = "synthesisr"),
  full.names = TRUE)

# now we can use read_refs to read in our bibliographic data files
df_initial <- read_refs(bibfiles)
```

## Deduplicate bibliographic data

Many journals are indexed in multiple databases, so searching across
databases will retrieve duplicates. After import, `synthesisr` can
detect duplicates and retain only unique bibliographic records using a
variety of methods such as string distance or fuzzy matching records. A
good place to start is removing articles that have identical DOIs,
because these are unique (by definition), and while not every entry will
have a DOI stored, removing them reduces the computational time required
for more sophisticated deduplication methods.

``` r
df <- deduplicate(df_initial) # uses DOI by default
```

In some cases, it may be useful to know which articles were identified
as duplicates so they can be manually reviewed or so that information
from two records can be merged. Using our partially-deduplicated
dataset, we check a few titles and use string distance methods to find
additional duplicate articles in the code below and then remove them by
extracting unique references. Although here we only use one secondary
deduplication method (string distance), we could look for additional
duplicates based on fuzzy matching abstracts, for example:

``` r
possible_duplicates <- find_duplicates(df$title,
                                       to_lower = TRUE,
                                       rm_punctuation = TRUE)


manual_checks <- review_duplicates(df$title, possible_duplicates)
print(manual_checks, n = 6)
#> # A tibble: 4 × 2
#>   title                                                                  matches
#>   <chr>                                                                    <int>
#> 1 Comparing bird assemblages in successional black spruce stands origin…      78
#> 2 Comparing bird assemblages in successional black spruce stands origin…      78
#> 3 Composition of Bird Communities Following Stand-Replacement Fires in …      82
#> 4 Composition of bird communities following stand-replacement fires in …      82
```

In this case, the examples look fine, so we accept them:

``` r
df <- extract_unique_references(df, possible_duplicates)
```

Exact string matching is a fairly coarse method, however, even when
we’ve taken some basic steps to avoid issues from differences in
capitalisation and punctuation. Having removed ‘obvious’ duplicates,
therefore, it may be wise to seek near-misses using string distances.

``` r
more_duplicates <- find_duplicates(df$title,
                                   method = "string_osa",
                                   to_lower = TRUE,
                                   rm_punctuation = TRUE)

review_duplicates(df$title, more_duplicates)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: title <chr>, matches <dbl>
```

Here we see that *why* our earlier approach had some issues. For
example, one of our examples had improper spacing:

``` r
df$title[more_duplicates == 21]
```

    #> An integrated occupancy and **space-use model** to predict abundance of
    #> imperfectly detected, territorial vertebrates
    #> An integrated occupancy and **space-usemodel** to predict abundance of
    #> imperfectly detected, territorial vertebrates

(emphasis added)

Similarly, different databases sometime add

``` r
df$title[more_duplicates == 140]
```

    #> Black-backed **three-toed wood-pecker**, Picoides arcticus, predation on
    #> Monochamus oregopensis(Coleoptera: Cerambycidae)
    #> Black-backed **three-toed woodpecker**, Pieoides arcticus, predation on
    #> Monochamus oregonensis (Coleoptera, Cerambycidae)

More problematically, however, entries that are deliberately named in a
similar way are detected as duplicates in this method:

``` r
df$title[more_duplicates == 99]
```

    #> **2006** May species count of birds
    #> **2002** May species count for birds

We can add these as exceptions manually before applying our changes:

``` r
new_duplicates <- override_duplicates(more_duplicates, 99)
#> Warning in max(which(matches == overrides[i])): no non-missing arguments to
#> max; returning -Inf
results <- extract_unique_references(df, new_duplicates)
```

This leaves us with 91 entries, down from the 226 returned by
[`read_refs()`](https://martinwestgate.com/synthesisr/reference/read_refs.md).

## Write bibliographic files

To facilitate exporting results to other platforms after assembly and
deduplication, `synthesisr` can write bibliographic data to .ris or .bib
files. Optionally,
[`write_refs()`](https://martinwestgate.com/synthesisr/reference/write_refs.md)
can write directly to a text file stored locally.

``` r
# # synthesisr can write the full dataset to a bibliographic file
# # but in this example, we will just write the first citation
# # we also want it to be a nice clean bibliographic file, so we remove NA data
# # this makes it easier to view the output when working with a single article
# citation <- df[1, !is.na(df[1,])]
# 
# format_citation(citation)
# 
# write_refs(citation,
#   format = "bib",
#   file = FALSE
# )
```
