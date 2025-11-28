# Import bibliographic data files

Import common bibliographic reference formats such as `.bib`, `.ris`, or
`.txt`.

## Usage

``` r
read_refs(
  filename,
  tag_naming = "best_guess",
  return_df = TRUE,
  verbose = FALSE,
  locale = vroom::default_locale(),
  ...
)
```

## Arguments

- filename:

  A path to a filename or vector of filenames containing search results
  to import.

- tag_naming:

  Either a length-1 character stating how should ris tags be replaced
  (see details for a list of options), or an object inheriting from
  class `tibble` containing user-defined replacement tags.

- return_df:

  If `TRUE` (default), returns a `tibble`; if `FALSE`, returns a list.

- verbose:

  If `TRUE`, prints status updates (defaults to `FALSE`).

- locale:

  passed to
  [`vroom::vroom_lines()`](https://vroom.r-lib.org/reference/vroom_lines.html)

- ...:

  Additional arguments, passed to
  [`vroom::vroom()`](https://vroom.r-lib.org/reference/vroom.html) or
  [`vroom::vroom_lines()`](https://vroom.r-lib.org/reference/vroom_lines.html)

## Value

Returns a `tibble` unless `return_df` is set to `FALSE`, when it returns
a `list`.

## Details

Accepted values for `tag_naming` are: '

- `"best_guess"`: estimate which database has been used for ris tag
  replacement, then fill any gaps with generic tags. Any tags missing
  from
  [code_lookup](https://martinwestgate.com/synthesisr/reference/code_lookup.md)
  are passed unchanged.

- `"wos"` Web of Science tags

- `"scopus"` Scopus tags

- `"ovid"` OVID tags

- `"asp"` Academic Search Premier tags

- `"none"` Do not rename tags

- A `tibble` with the following columns:

  - `"code"` listing the original tags in the source document

  - `"field"` listing the replacement column/tag names

  - `"order"` (optional) listing the order of columns in the resulting
    `tibble`

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
}")

tmp <- tempfile()
writeLines(litsearchr, tmp)
df <- read_refs(tmp, return_df = TRUE, verbose = TRUE)
#> Reading file /tmp/RtmpvqJqrb/file194d12857dd6 ... done
```
