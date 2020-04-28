title <- "On the Origin of Species by Means of Natural Selection, or the Preservation of Favoured Races in the Struggle for Life"

x <- add_line_breaks(title, n = 20)

x2 <- strsplit(x, "\n")[[1]]
lapply(x2, nchar)

"On the Origin of Species\nby Means of Natural\nSelection, or the\nPreservation of Favoured Races in\nthe Struggle for Life"
