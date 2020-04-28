title <- "On the Origin of Species by Means of Natural Selection, or the Preservation of Favoured Races in the Struggle for Life"

lines_added <- add_line_breaks(title, n = 20)

split_text <- strsplit(lines_added, "\n")[[1]]

nchars <- unlist(lapply(gsub(" ", "", split_text), nchar))

expect((all(nchars)<20), failure_message = "Expected number of characters not found in groups split by add_line_breaks")
