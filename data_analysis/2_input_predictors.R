# This script runs the statistical analyses reported in the section
# "Predicting language outcomes with children’s language input."

library(tidyverse)
data <- read_csv("data/seedlings_data.csv")

#### CORRELATIONS ####

celf_tokens_cor <- cor.test(data$celf_core_lang, data$mean_tokens,
                           method = "spearman")

celf_types_cor <- cor.test(data$celf_core_lang, data$mean_types,
                            method = "spearman")

celf_op_cor <- cor.test(data$celf_core_lang, data$prop_op,
                           method = "spearman")

celf_propi_cor <- cor.test(data$celf_core_lang, data$propi,
                        method = "spearman")

celf_propn_cor <- cor.test(data$celf_core_lang, data$propn,
                           method = "spearman")

celf_propr_cor <- cor.test(data$celf_core_lang, data$propr,
                           method = "spearman")

celf_mean_awc_cor <- cor.test(data$celf_core_lang, data$mean_awc_perhr,
                             method = "spearman")

celf_sf5_awc_cor <- cor.test(data$celf_core_lang, data$sf5_awc_perhr,
                              method = "spearman")

# Pearson's correlations
celf_tokens_cor2 <- cor.test(data$celf_core_lang, data$mean_tokens,
                            method = "pearson")

celf_types_cor2 <- cor.test(data$celf_core_lang, data$mean_types,
                           method = "pearson")

celf_op_cor2 <- cor.test(data$celf_core_lang, data$prop_op,
                        method = "pearson")

celf_propi_cor2 <- cor.test(data$celf_core_lang, data$propi,
                           method = "pearson")

celf_propn_cor2 <- cor.test(data$celf_core_lang, data$propn,
                           method = "pearson")

celf_propr_cor2 <- cor.test(data$celf_core_lang, data$propr,
                           method = "pearson")

celf_mean_awc_cor2 <- cor.test(data$celf_core_lang, data$mean_awc_perhr,
                              method = "pearson")

celf_sf5_awc_cor2 <- cor.test(data$celf_core_lang, data$sf5_awc_perhr,
                             method = "pearson")

#### MODEL ####

mod_input <- lm(data = data, celf_core_lang ~ propi)
