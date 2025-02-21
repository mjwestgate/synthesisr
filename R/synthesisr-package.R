#' synthesisr: Import, assemble, and deduplicate bibiliographic datasets
#'
#'  Systematic review searches include multiple databases
#'  that export results in a variety of formats with overlap in
#'  coverage between databases. To streamline the process of importing,
#'  assembling, and deduplicating results, `synthesisr` recognizes
#'  bibliographic files exported from databases commonly used for
#'  systematic reviews and merges results into a standardized format.
#'
#' @section Import & Export:
#' The key task performed by `synthesisr` is flexible import and
#' presentation of bibliographic data. This is typically achieved by
#' `read_refs()`, which can import multiple files at once and link them together
#' into a single `data.frame`. Conversely, export is via `write_refs()`. Users
#' that require more detailed control can use the following functions:
#'
#' \itemize{
#'   \item [read_refs] Read bibliographic data
#'   \item [write_refs] Write bibliographic data
#'   \item [detect_] Detect file attributes
#'   \item [parse_] Parse a vector containing bibliographic data
#'   \item [clean_] Cleaning functions for author and column names
#'   \item [code_lookup] A dataset of potential ris tags
#' }
#'
#' @section Formatting:
#' \itemize{
#'   \item [bibliography-class] Methods for class `bibliography`
#'   \item [format_citation] Return a clean citation from a `bibliography` or `data.frame`
#'   \item [add_line_breaks] Set a maximum character width for strings
#'}
#'
#' @section Deduplication:
#' When importing from multiple databases, it is likely that there will be
#' duplicates in the resulting dataset. The easiest way to deal with this
#' problem in `synthesisr` is using the `deduplicate()` function; but this can
#' be risky, particularly if there are no DOIs in the dataset. To get finer
#' control of the deduplication process, consider using the sub-functions:
#'
#'\itemize{
#'   \item [deduplicate] Semi-automated duplicate removal
#'   \item [find_duplicates] Locate potentially duplicated references
#'   \item [extract_unique_references] Return a data.frame with only 'unique' references
#'   \item [review_duplicates] Manually review potential duplicates
#'   \item [override_duplicates] Manually override identified duplicates
#'   \item [fuzz_] Fuzzy string matching c/o `fuzzywuzzy`
#'   \item [string_] Fuzzy string matching c/o `stringdist`
#'}
#'
#' @section Deprecated:
#'
#' \itemize{
#'   \item [merge_columns] Synonymous with [dplyr::bind_rows]
#' }
#' @name synthesisr-package
#' @docType package
"_PACKAGE"
