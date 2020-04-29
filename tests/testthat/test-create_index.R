expect(all(create_index(n=3)==c("V_1", "V_2", "V_3")),
       "Default string not being added to index")

expect(all(create_index(string="var", n=3, sep=".")==c("var.1", "var.2", "var.3")),
       "Custom string and separator not being added to index")
