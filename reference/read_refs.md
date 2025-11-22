# Import bibliographic search results

Import common bibliographic reference formats such as `.bib`, `.ris`, or
`.txt`.

## Usage

``` r
read_refs(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE,
  locale = vroom::default_locale()
)
```

## Arguments

- filename:

  A path to a filename or vector of filenames containing search results
  to import.

- tag_naming:

  Either a length-1 character stating how should ris tags be replaced
  (see details for a list of options), or an object inheriting from
  class `data.frame` containing user-defined replacement tags.

- return_df:

  If `TRUE` (default), returns a `data.frame`; if `FALSE`, returns a
  list.

- verbose:

  If `TRUE`, prints status updates (defaults to `FALSE`).

## Value

Returns a `data.frame` or `list` of assembled search results.

## Details

The default for argument `tag_naming` is `"best_guess"`, which estimates
what database has been used for ris tag replacement, then fills any gaps
with generic tags. Any tags missing from the database (i.e.
`code_lookup`) are passed unchanged. Other options are to use tags from
Web of Science (`"wos"`), Scopus (`"scopus"`), Ovid (`"ovid"`) or
Academic Search Premier (`"asp"`). If a `data.frame` is given, then it
must contain two columns: `"code"` listing the original tags in the
source document, and `"field"` listing the replacement column/tag names.
The `data.frame` may optionally include a third column named `"order"`,
which specifies the order of columns in the resulting `data.frame`;
otherwise this will be taken as the row order. Finally, passing `"none"`
to `replace_tags` suppresses tag replacement.

## Examples

``` r
litsearchr <- c(
  "@article{grames2019,
  title={An automated approach to identifying search terms for
  systematic reviews using keyword co-occurrence networks},
  author={Grames, Eliza M and Stillman, Andrew N and Tingley, Morgan W and Elphick, Chris S},
  journal={Methods in Ecology and Evolution},
  volume={10},
  number={10},
  pages={1645--1654},
  year={2019},
  publisher={Wiley Online Library}
}"
)

tmp <- tempfile()

writeLines(litsearchr, tmp)

df <- read_refs(tmp, return_df = TRUE, verbose = TRUE)
#> Reading file /tmp/RtmpIJAhzk/file18ee8e39010 ... done
```
