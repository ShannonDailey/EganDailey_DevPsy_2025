# This script aggregates the data frame used for the supplemental materials.

data <- read_csv("data/seedlings_data.csv")
 
demo <- read_csv("data/supplement/demographics.csv")

extra_measures <- read_csv("data/supplement/extra_measures_data.csv")

supplement_data <- data %>%
  left_join(demo) %>%
  left_join(extra_measures)

write_csv(supplement_data, "data/supplement/supplement_data.csv")
