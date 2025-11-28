# synthesisr: Import, assemble, and deduplicate bibiliographic datasets

Systematic review searches include multiple databases that export
results in a variety of formats with overlap in coverage between
databases. To streamline the process of importing, assembling, and
deduplicating results, `synthesisr` recognizes bibliographic files
exported from databases commonly used for systematic reviews and merges
results into a standardized format.

## Import & Export

The key task performed by `synthesisr` is flexible import and
presentation of bibliographic data. This is typically achieved by
[`read_refs()`](https://martinwestgate.com/synthesisr/reference/read_refs.md),
which can import multiple files at once and link them together into a
single `tibble`. Conversely, export is via
[`write_refs()`](https://martinwestgate.com/synthesisr/reference/write_refs.md).
Users that require more detailed control can use the following
functions:

- [read_refs](https://martinwestgate.com/synthesisr/reference/read_refs.md)
  Read bibliographic data

- [write_refs](https://martinwestgate.com/synthesisr/reference/write_refs.md)
  Write bibliographic data

- [detect\_](https://martinwestgate.com/synthesisr/reference/detect_.md)
  Detect file attributes

- [parse\_](https://martinwestgate.com/synthesisr/reference/parse_.md)
  Parse a vector containing bibliographic data

- [clean\_](https://martinwestgate.com/synthesisr/reference/clean_.md)
  Cleaning functions for author and column names

- [code_lookup](https://martinwestgate.com/synthesisr/reference/code_lookup.md)
  A dataset of potential ris tags

## Formatting

- [bibliography](https://martinwestgate.com/synthesisr/reference/bibliography-class.md)
  Methods for class `bibliography`

- [format_citation](https://martinwestgate.com/synthesisr/reference/format_citation.md)
  Return a clean citation from a `bibliography` or `data.frame`

- [add_line_breaks](https://martinwestgate.com/synthesisr/reference/add_line_breaks.md)
  Set a maximum character width for strings

## Deduplication

When importing from multiple databases, it is likely that there will be
duplicates in the resulting dataset. The easiest way to deal with this
problem in `synthesisr` is using the
[`deduplicate()`](https://martinwestgate.com/synthesisr/reference/deduplicate.md)
function; but this can be risky, particularly if there are no DOIs in
the dataset. To get finer control of the deduplication process, consider
using the sub-functions:

- [deduplicate](https://martinwestgate.com/synthesisr/reference/deduplicate.md)
  Semi-automated duplicate removal

- [find_duplicates](https://martinwestgate.com/synthesisr/reference/find_duplicates.md)
  Locate potentially duplicated references

- [extract_unique_references](https://martinwestgate.com/synthesisr/reference/extract_unique_references.md)
  Return a data.frame with only 'unique' references

- [review_duplicates](https://martinwestgate.com/synthesisr/reference/review_duplicates.md)
  Manually review potential duplicates

- [override_duplicates](https://martinwestgate.com/synthesisr/reference/override_duplicates.md)
  Manually override identified duplicates

- [fuzz\_](https://martinwestgate.com/synthesisr/reference/fuzz_.md)
  Fuzzy string matching c/o `fuzzywuzzy`

- [string\_](https://martinwestgate.com/synthesisr/reference/string_.md)
  Fuzzy string matching c/o `stringdist`

## See also

Useful links:

- <https://martinwestgate.com/synthesisr/>

## Author

**Maintainer**: Martin Westgate <martinjwestgate@gmail.com>
([ORCID](https://orcid.org/0000-0003-0854-2034))

Authors:

- Eliza Grames <eliza.grames@uconn.edu>
  ([ORCID](https://orcid.org/0000-0003-1743-6815))
