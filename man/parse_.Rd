% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_bibtex.R, R/parse_csv_tsv.R,
%   R/parse_pubmed.R, R/parse_ris.R
\name{parse_bibtex}
\alias{parse_bibtex}
\alias{parse_csv}
\alias{parse_tsv}
\alias{parse_}
\alias{parse_pubmed}
\alias{parse_ris}
\title{Parse bibliographic text in a variety of formats}
\usage{
parse_bibtex(x)

parse_csv(x)

parse_tsv(x)

parse_pubmed(x)

parse_ris(x, tag_naming = "best_guess")
}
\arguments{
\item{x}{A character vector containing bibliographic information in ris
format.}

\item{tag_naming}{What format are ris tags in? Defaults to \code{"best_guess"} See
\code{read_refs()} for a list of accepted arguments.}
}
\value{
Returns an object of class \code{bibliography} (ris, bib, or pubmed
formats) or \code{data.frame} (csv or tsv).
}
\description{
Text in standard formats - such as imported via
\code{base::readLines()} - can be parsed using a variety of standard formats. Use
\code{detect_parser()} to determine which is the most appropriate parser for your
situation. Note that \code{parse_tsv()} and \code{parse_csv()} are maintained for
backwards compatability only; within \code{read_ref} these have been replaced
by \code{vroom::vroom()}.
}
\examples{
eviatlas <- c(
  "TY  - JOUR",
  "AU  - Haddaway, Neal R.",
  "AU  - Feierman, Andrew",
  "AU  - Grainger, Matthew J.",
  "AU  - Gray, Charles T.",
  "AU  - Tanriver-Ayder, Ezgi",
  "AU  - Dhaubanjar, Sanita",
  "AU  - Westgate, Martin J.",
  "PY  - 2019",
  "DA  - 2019/06/04",
  "TI  - EviAtlas: a tool for visualising evidence synthesis databases",
  "JO  - Environmental Evidence",
  "SP  - 22",
  "VL  - 8",
  "IS  - 1",
  "SN  - 2047-2382",
  "UR  - https://doi.org/10.1186/s13750-019-0167-1",
  "DO  - 10.1186/s13750-019-0167-1",
  "ID  - Haddaway2019",
  "ER  - "
)

detect_parser(eviatlas) # = "parse_ris"
df <- as.data.frame(parse_ris(eviatlas))
ris_out <- write_refs(df, format = "ris", file = FALSE)
}
