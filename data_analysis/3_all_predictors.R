# This script runs the statistical analyses reported in the section
# "Combining language skills and input from infancy to predict preschool language skills."

library(tidyverse)
data <- read_csv("data/seedlings_data.csv")

mod_data <- data %>%
  drop_na(cdi_18mo,
          quils_overall_standard,
          sf3_pvt_AgeCorrectedScore,
          propi)

mod_celf_big <- lm(data = mod_data, celf_core_lang ~
                     cdi_18mo +
                     quils_overall_standard +
                     sf3_pvt_AgeCorrectedScore +
                     propi)

step <- MASS::stepAIC(mod_celf_big, direction = "backward")

celf_best_mod <- lm(celf_core_lang ~ cdi_18mo + quils_overall_standard,
                    data = data)

celf_cdi_only <- lm(celf_core_lang ~ cdi_18mo,
                       data = data)
