library(rfishbase)
library(magrittr)

names <- read.csv("data/processed/fixed_species_names.csv") %$% x %>%
  as.character()

maturity_table <- maturity(names)

write.csv(maturity_table, "data/processed/maturity_table.csv", row.names = F)
