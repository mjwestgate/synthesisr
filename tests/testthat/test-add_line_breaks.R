title <-
  "On the Origin of Species by Means of Natural Selection, or the Preservation of Favoured Races in the Struggle for Life"
N <- 20
lines_added <- add_line_breaks(title, n = N)

split_text <- strsplit(lines_added, "\n")[[1]]

nchars <- unlist(lapply(gsub(" ", "", split_text), nchar))

expect((all(nchars) < N), failure_message = "Expected number of characters not found in groups split by add_line_breaks")
