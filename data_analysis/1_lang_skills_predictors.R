# This script runs the statistical analyses reported in the section
# "Predicting language outcomes with children’s previous language skills."

library(tidyverse)
data <- read_csv("data/seedlings_data.csv")

#### Correlations ####

celf_cdi12_cor <- cor.test(data$celf_core_lang, data$cdi_12mo,
                           method = "spearman")

celf_cdi18_cor <- cor.test(data$celf_core_lang, data$cdi_18mo,
                           method = "spearman")

celf_quils_cor <- cor.test(data$celf_core_lang, data$quils_overall_standard,
                           method = "spearman")

celf_tpvt3_cor <- cor.test(data$celf_core_lang, data$sf3_pvt_AgeCorrectedScore,
                           method = "spearman")

# Pearson's correlations
celf_cdi12_cor2 <- cor.test(data$celf_core_lang, data$cdi_12mo,
                           method = "pearson")

celf_cdi18_cor2 <- cor.test(data$celf_core_lang, data$cdi_18mo,
                           method = "pearson")

celf_quils_cor2 <- cor.test(data$celf_core_lang, data$quils_overall_standard,
                           method = "pearson")

celf_tpvt3_cor2 <- cor.test(data$celf_core_lang, data$sf3_pvt_AgeCorrectedScore,
                           method = "pearson")

#### Model ####

mod_skills <- lm(data = data, celf_core_lang ~ cdi_18mo + quils_overall_standard + sf3_pvt_AgeCorrectedScore)
