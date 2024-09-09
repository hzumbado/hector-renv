library(dplyr)
library(tidyr)
library(data.table)

#### ADD CALL TO LIBRARY (ONLY DATATABLE SCRIPT) *******************************

# Load data----
data <- fread(file = "data/raw-data.tsv.gz",
              sep = "\t",
              header = TRUE,
              na.strings = ":")

# Load data----
#### CHANGE WITH DATATABLE-SCRIPT CODE *****************************************
data <- read.table(gzfile("data/raw-data.tsv.gz"),
                   header = TRUE,
                   sep = "\t",
                   na.strings = ":",
                   check.names = FALSE,
                   strip.white = TRUE)
#### ***************************************************************************

# Clean data----
# Countries in the Schengen area in 2021
countries <- c("AT", "BE", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IS",
               "IT", "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", "SK",
               "SI", "ES", "SE", "CH")

cleaned_data <- data |>
  separate_wider_delim(cols = starts_with("unit"),
                       delim = ",",
                       names = c("unit", "age", "sex", "country")) |>
  filter(!age %in% c("TOTAL", "UNK"),
         sex != "T",
         country %in% countries) |>
  mutate(across(!1:4, \(x) stringr::str_extract(x, pattern = "\\d+(\\.\\d+)?")),
         across(!1:4, as.integer),
         country = ifelse(country == "EL", "GR", country)) |>
  pivot_longer(cols = !1:4,
               names_to = "year",
               values_to = "pop",
               names_transform = as.integer) |>
  select(country, year, age, sex, pop)

# Save cleaned data
cleaned_data |>
  write.csv(file = "data/cleaned-data.csv",
            row.names = FALSE)
