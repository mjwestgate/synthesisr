# Format a citation

This function takes an object of class `data.frame`, `list`, or
`bibliography` and returns a formatted citation.

## Usage

``` r
format_citation(
  data,
  details = TRUE,
  abstract = FALSE,
  add_html = FALSE,
  line_breaks = FALSE,
  ...
)
```

## Arguments

- data:

  An object of class `data.frame`, `list`, or `bibliography.`

- details:

  Logical: Should identifying information such as author names & journal
  titles be displayed? Defaults to `TRUE`.

- abstract:

  Logical: Should the abstract be shown (if available)? Defaults to
  `FALSE.`

- add_html:

  Logical: Should the journal title be italicized using html codes?
  Defaults to `FALSE`.

- line_breaks:

  Either logical, stating whether line breaks should be added, or
  numeric stating how many characters should separate consecutive line
  breaks. Defaults to `FALSE`.

- ...:

  any other arguments.

## Value

Returns a string of length equal to `length(data)` that contains
formatted citations.

## Examples

``` r
roses <- c("@article{haddaway2018,
  title={ROSES RepOrting standards for Systematic Evidence Syntheses:
  pro forma, flow-diagram and descriptive summary of the plan and
  conduct of environmental systematic reviews and systematic maps},
  author={Haddaway, Neal R and Macura, Biljana and Whaley, Paul and Pullin, Andrew S},
  journal={Environmental Evidence},
  volume={7},
  number={1},
  pages={7},
  year={2018},
  publisher={Springer}
}")

tmp <- tempfile()
writeLines(roses, tmp)

citation <- read_ref(tmp)
#> Error in read_ref(tmp): could not find function "read_ref"
format_citation(citation)
#> Error in format_citation(citation): format_citation expects input data to be an object of class data.frame, bibliography, or list
```
