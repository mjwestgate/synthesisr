# Bibliographic code lookup for search results assembly

A `tibble` that can be used to look up common codes for different
bibliographic fields across databases and merge them to a common format.

## Usage

``` r
code_lookup
```

## Format

A `tibble` with 226 obs of 12 variables

- code:

  code used in search results

- order:

  the order in which to rank fields in assembled results

- category_description:

  type of bibliographic data

- entry_description:

  description of field

- field:

  bibliographic field that codes correspond to

- ris_generic:

  logical: If the code is used in generic ris files

- ris_wos:

  logical: If the code is used in Web of Science ris files

- ris_pubmed:

  logical: If the code is used in PubMed ris files

- ris_scopus:

  logical: If the code is used in Scopus ris files

- ris_asp:

  logical: If the code is used in Academic Search Premier ris files

- ris_ovid:

  logical: If the code is used in Ovid ris files

- ris_synthesisr:

  logical: If the code used in synthesisr imports & exports
