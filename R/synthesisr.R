#' synthesisr: Import, Assemble, and Deduplicate Bibiliographic Datasets
#'
#'  Systematic review searches include multiple databases
#'  that export results in a variety of formats with overlap in
#'  coverage between databases. To streamline the process of importing,
#'  assembling, and deduplicating results, synthesisr recognizes
#'  bibliographic files exported from databases commonly used for
#'  systematic reviews and merges results into a standardized format.
#'
#' @section Import:
#' The key task performed by \code{synthesisr} is flexible import and presentation of bibliographic data. This is typically achieved by \code{\link{read_refs}}, which can import multiple files at once and link them together into a single \code{data.frame}. Users that require more detailed control can use the following functions:
#' \itemize{
#'   \item \code{\link{read_ref}} Import an individual bibliographic dataset
#'   \item \code{\link{detect_format}} Detects if a file is bib-like or ris-like
#'   \item \code{\link{detect_delimiter}} Detect delimiter type in ris files
#'   \item \code{\link{code_lookup}} Tag replacement for ris files
#'   \item \code{\link{parse_ris}} Parse a vector containing bibliographic data
#'   \item \code{\link{as.bibliography}} Methods for class 'bibliography'
#'   \item \code{\link{match_columns}} Takes an imported data.frame and rearranges it to match lookup codes
#'   \item \code{\link{clean_df}} Clean author and column names
#'   \item \code{\link{generate_ids}} Generate unique row IDs from bibliographic data
#' }
#'
#' @section Deduplication:
#' When importing from multiple databases, it is likely that there will be duplicates in the resulting dataset. The easiest way to deal with this problem in \code{synthesisr} is using the \code{\link{deduplicate}} command; but this can be risky, particularly if there are no DOIs in the dataset. To get finer control of the deduplication process, consider using the sub-functions:
#'\itemize{
#'   \item \code{\link{find_duplicates}} Locate potentially duplicated references
#'   \item \code{\link{extract_unique_references}} Return a data.frame with only 'unique' references
#'   \item \code{\link{fuzzdist}} Fuzzy string matching
#'   \item \code{\link{review_duplicates}} Manually review potential duplicates
#'}
#'
#' @section Export:
#' Exporting data processed in \code{synthesisr} back to a file in a standard bibliographic format is possible using \code{\link{write_refs}}. Alternatively, you can call either of the two sub-functions to export to ris or bibtex formats (respectively):
#' \itemize{
#'   \item \code{\link{write_ris}} Export as ris
#'   \item \code{\link{write_bib}} Export as bib
#' }
#'
#' @section Miscellaneous Functions:
#' \code{synthesisr} contains several functions that are useful for manipulating data; these are included here for completeness.
#' \itemize{
#'  \item \code{\link{merge_columns}} rbind two data.frames with different numbers of columns
#'  \item \code{\link{remove_factors}} Remove factors from a data.frame
#'  \item \code{\link{create_index}} Create an ordered string
#'  \item \code{\link{format_citation}} Return a clean citation from a bibliography or data.frame
#'  \item \code{\link{add_line_breaks}} Set a maximum character width for strings
#'}
#'
#' @docType package
#' @name synthesisr

# quiet no visible binding NOTEs
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @useDynLib
#' @importFromRcpp sourceCpp
