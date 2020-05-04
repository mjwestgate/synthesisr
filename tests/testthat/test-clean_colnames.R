cleaned <- clean_colnames(c(".title...", "X..YEAR",
                         "authors..", ".AUTHOR"))

expect(!any(grepl("[[:punct::]]", cleaned)), "Punctuation not removed from names")

expect(!any(duplicated(cleaned)), "Duplicated names not cleaned")

expect(!any(grepl("^(X|Y|Z)\\.+", cleaned)), "Leading X not removed from names")
