# This script runs the analyses reported in the Supplementary Materials.

library(tidyverse)
library(broom)
library(papaja)
#install.packages("corx")
library(corx)

supplement_data <- read_csv("data/supplement/supplement_data.csv")

#### 1. Descriptive statistics for language input and skill measures ####

# Table S1

summary_data <- supplement_data %>%
  dplyr::select(subj, celf_core_lang,
                cdi_12mo, cdi_18mo, quils_overall_standard, sf3_pvt_AgeCorrectedScore,
                mean_tokens, mean_types, prop_op, propi, propn, propr,
                mean_awc_perhr, sf5_awc_perhr) %>%
  pivot_longer(cols = c(celf_core_lang,
                        cdi_12mo, cdi_18mo, quils_overall_standard, sf3_pvt_AgeCorrectedScore,
                        mean_tokens, mean_types, prop_op, propi, propn, propr,
                        mean_awc_perhr, sf5_awc_perhr),
               names_to = "Measure",
               values_to = "value")

table_s1_data <- summary_data %>%
  group_by(Measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            SD = sd(value, na.rm = TRUE),
            Median = median(value, na.rm = TRUE),
            Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE),
            n = sum(!is.na(value))) %>%
  mutate(Measure = ordered(Measure,
                           levels = c("mean_tokens", "mean_types", "prop_op",
                                      "propi", "propn", "propr",
                                      "mean_awc_perhr", "sf5_awc_perhr",
                                      "cdi_12mo", "cdi_18mo",
                                      "quils_overall_standard", "sf3_pvt_AgeCorrectedScore", "celf_core_lang"))) %>% 
  arrange(Measure) %>% 
  mutate(child_age = case_when(Measure %in% c("mean_awc_perhr", "mean_tokens", "mean_types",
                                              "prop_op", "propi", "propn", "propr") ~ "0;6-1;5",
                               Measure == "cdi_12mo" ~ "1;0",
                               Measure == "cdi_18mo" ~ "1;6",
                               Measure %in% c("quils_overall_standard", "sf3_pvt_AgeCorrectedScore") ~ "3;6",
                               Measure %in% c("celf_core_lang", "sf5_awc_perhr") ~ "4;6")) %>% 
  mutate(Measure = recode_factor(Measure,
                                 cdi_12mo = "Productive MCDI",
                                 cdi_18mo = "Productive MCDI",
                                 celf_core_lang = "CELF-P 2 core language skill",
                                 mean_awc_perhr = "LENA AWC",
                                 mean_tokens = "Noun tokens",
                                 mean_types = "Noun types",
                                 prop_op = "Prop. object present",
                                 propi = "Prop. imperatives",
                                 propn = "Prop. short phrases",
                                 propr = "Prop. reading",
                                 quils_overall_standard = "QUILS",
                                 sf3_pvt_AgeCorrectedScore = "TPVT",
                                 sf5_awc_perhr = "LENA AWC")) %>%
  dplyr::select(Measure, child_age, Mean:n) %>%
  rename("Child age" = "child_age")

# Table S2
data_for_corrs <- supplement_data %>%
  dplyr::select(celf_core_lang,
                cdi_12mo, cdi_18mo, quils_overall_standard, sf3_pvt_AgeCorrectedScore,
                mean_tokens, mean_types, prop_op, propi, propn, propr,
                mean_awc_perhr, sf5_awc_perhr) %>%
  rename("CELF-P2 core language skill (4;6)" = "celf_core_lang",
         "Productive MCDI (1;0)" = "cdi_12mo",
         "Productive MCDI (1;6)" = "cdi_18mo",
         "QUILS (3;6)" = "quils_overall_standard",
         "TPVT (3;6)" = "sf3_pvt_AgeCorrectedScore",
         "Noun tokens (0;6-1;5)" =  "mean_tokens",
         "Noun types (0;6-1;5)" = "mean_types",
         "Prop. object present (0;6-1;5)" = "prop_op",
         "Prop. imperatives (0;6-1;5)" = "propi",
         "Prop. short phrases (0;6-1;5)" = "propn",
         "Prop. reading (0;6-1;5)" = "propr",
         "LENA AWC (0;6-1;5)" = "mean_awc_perhr",
         "LENA AWC (4;6)" = "sf5_awc_perhr")

table_s2_data <- corx(as.matrix(data_for_corrs),
                   triangle = "lower",
                   stars = c(0.05, 0.01, 0.001))

#### 2. Supplementary analysis controlling for maternal education and child gender ####

# Relationship between maternal education (categorical) and CELF core language
celf_edu_anova <- aov(celf_core_lang ~ mat_ed, data = supplement_data) %>%
  apa_print()

# Relationship between maternal education (continuous) and CELF core language
celf_edu_cor <- cor.test(supplement_data$celf_core_lang, supplement_data$mat_ed_yrs,
                           method = "spearman")

# Model (categorical maternal education)
mod_data_demo <- supplement_data %>%
  drop_na(cdi_18mo,
          quils_overall_standard,
          sf3_pvt_AgeCorrectedScore,
          propi,
          mat_ed,
          gender)

mod_w_demo <- lm(data = mod_data_demo, celf_core_lang ~
                   cdi_18mo +
                   quils_overall_standard +
                   sf3_pvt_AgeCorrectedScore +
                   propi +
                   mat_ed +
                   gender)

step_demo <- MASS::stepAIC(mod_w_demo, direction = "backward")

celf_best_mod_demo <- lm(celf_core_lang ~ cdi_18mo + mat_ed,
                         data = mod_data_demo)

# Model (continuous maternal education)
mod_w_demo_cont <- lm(data = mod_data_demo, celf_core_lang ~
                        cdi_18mo +
                        quils_overall_standard +
                        sf3_pvt_AgeCorrectedScore +
                        propi +
                        mat_ed_yrs +
                        gender)

step_demo2 <- MASS::stepAIC(mod_w_demo_cont, direction = "backward")

celf_best_mod_demo2 <- lm(celf_core_lang ~ cdi_18mo + quils_overall_standard,
                          data = mod_data_demo)

#### 3. Supplementary analysis predicting MCDI vocabulary at 1;6 ####

# CORRELATIONS

cdi_tokens_cor <- cor.test(supplement_data$cdi_18mo, supplement_data$mean_tokens,
                           method = "spearman")

cdi_types_cor <- cor.test(supplement_data$cdi_18mo, supplement_data$mean_types,
                          method = "spearman")

cdi_op_cor <- cor.test(supplement_data$cdi_18mo, supplement_data$prop_op,
                       method = "spearman")

cdi_propi_cor <- cor.test(supplement_data$cdi_18mo, supplement_data$propi,
                          method = "spearman")

cdi_propn_cor <- cor.test(supplement_data$cdi_18mo, supplement_data$propn,
                          method = "spearman")

cdi_propr_cor <- cor.test(supplement_data$cdi_18mo, supplement_data$propr,
                          method = "spearman")

cdi_mean_awc_cor <- cor.test(supplement_data$cdi_18mo, supplement_data$mean_awc_perhr,
                             method = "spearman")

# MODEL

mod_input_18mo <- lm(data = supplement_data, cdi_18mo ~ mean_tokens)

#### 4. Additional measures of utterance types in noun input ####

celf_propq_cor <- cor.test(supplement_data$celf_core_lang, supplement_data$propq,
                           method = "spearman")

celf_propd_cor <- cor.test(supplement_data$celf_core_lang, supplement_data$propd,
                           method = "spearman")

celf_props_cor <- cor.test(supplement_data$celf_core_lang, supplement_data$props,
                           method = "spearman")

#### 5. Additional assessments of language and cognitive skills ####

extra_measures_data <- supplement_data %>%
  dplyr::select(subj, asq_Communication_sf3, psmt_AgeCorrectedScore,
                mefs_StandardScore, asq_Communication_sf5, wppsi4_bd_ss,
                wppsi4_mr_ss, pvt_AgeCorrectedScore_sf5,
                bus_info_standard, bus_len_standard, bus_com, celf_core_lang) %>%
  pivot_longer(cols = c(asq_Communication_sf3, asq_Communication_sf5,
                        psmt_AgeCorrectedScore, mefs_StandardScore,
                        wppsi4_bd_ss, wppsi4_mr_ss, pvt_AgeCorrectedScore_sf5,
                        bus_info_standard, bus_len_standard, bus_com),
               names_to = "Measure",
               values_to = "value")

# Correlations
celf_corr <- function(var1, var2){
  corr <- cor.test(var2, var1, method = "pearson") %>% tidy()
  paste0("R=", round(corr$estimate, 2), ", p=", printp(corr$p.value))
}

extra_measures_corrs <- supplement_data %>%
  summarise(asq_Communication_sf3 = celf_corr(.$celf_core_lang, .$asq_Communication_sf3),
            psmt_AgeCorrectedScore = celf_corr(.$celf_core_lang, .$psmt_AgeCorrectedScore),
            mefs_StandardScore = celf_corr(.$celf_core_lang, .$mefs_StandardScore),
            asq_Communication_sf5 = celf_corr(.$celf_core_lang, .$asq_Communication_sf5),
            wppsi4_bd_ss = celf_corr(.$celf_core_lang, .$wppsi4_bd_ss),
            wppsi4_mr_ss = celf_corr(.$celf_core_lang, .$wppsi4_mr_ss),
            pvt_AgeCorrectedScore_sf5 = celf_corr(.$celf_core_lang, .$pvt_AgeCorrectedScore_sf5),
            bus_info_standard = celf_corr(.$celf_core_lang, .$bus_info_standard),
            bus_len_standard = celf_corr(.$celf_core_lang, .$bus_len_standard),
            bus_com = celf_corr(.$celf_core_lang, .$bus_com)) %>%
  pivot_longer(cols = c(asq_Communication_sf3:bus_com),
               names_to = "Measure",
               values_to = "corr")

# Assembling table
table_s5_data <- extra_measures_data %>%
  group_by(Measure) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            SD = sd(value, na.rm = TRUE),
            Median = median(value, na.rm = TRUE),
            Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE),
            n = sum(!is.na(value))) %>%
  left_join(extra_measures_corrs) %>% 
  mutate(Measure = ordered(Measure,
                           levels = c("asq_Communication_sf3", "psmt_AgeCorrectedScore", "mefs_StandardScore",
                                      "asq_Communication_sf5", "wppsi4_bd_ss", "wppsi4_mr_ss",
                                      "pvt_AgeCorrectedScore_sf5", "bus_info_standard",
                                      "bus_len_standard", "bus_com"))) %>% 
  arrange(Measure) %>% 
  mutate(child_age = case_when(Measure %in% c("asq_Communication_sf3",
                                              "psmt_AgeCorrectedScore",
                                              "mefs_StandardScore") ~ "3;6",
                               Measure %in% c("asq_Communication_sf5",
                                              "wppsi4_bd_ss", "wppsi4_mr_ss",
                                              "pvt_AgeCorrectedScore_sf5",
                                              "bus_info_standard",
                                              "bus_len_standard", "bus_com") ~ "4;6")) %>% 
  mutate(Measure = ordered(Measure,
                           levels = c("asq_Communication_sf3",
                                      "asq_Communication_sf5", "pvt_AgeCorrectedScore_sf5",
                                      "bus_info_standard", "bus_len_standard", "bus_com",
                                      
                                      "psmt_AgeCorrectedScore", "mefs_StandardScore",
                                      "wppsi4_bd_ss", "wppsi4_mr_ss"))) %>% 
  arrange(Measure) %>% 
  mutate(Measure = fct_recode(Measure,
                              "ASQ-3 Communication" = "asq_Communication_sf3",
                              "Picture Sequence Memory Test (age-corrected)" = "psmt_AgeCorrectedScore",
                              "Minnestota Executive Function Scale (standard)" = "mefs_StandardScore",
                              "ASQ-3 Communication" = "asq_Communication_sf5",
                              "WPPSI-IV block design (standard)" = "wppsi4_bd_ss",
                              "WPPSI-IV matrix reasoning (standard)" = "wppsi4_mr_ss",
                              "TPVT (age-corrected)" = "pvt_AgeCorrectedScore_sf5",
                              "Renfrew Bus Story information (standard)" = "bus_info_standard",
                              "Renfrew Bus Story length (standard)" = "bus_len_standard",
                              "Renfrew Bus Story complexity" = "bus_com")) %>% 
  dplyr::select(Measure, child_age, Mean:n, corr) %>%
  rename("Child age" = "child_age",
         "Corr. w/ CELF-P 2" = "corr")

#### 6. Supplementary analysis predicting vocabulary outcome measures ####

# CELF vocabulary correlations
celfvoc_tokens_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$mean_tokens,
                            method = "spearman")

celfvoc_types_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$mean_types,
                           method = "spearman")

celfvoc_op_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$prop_op,
                        method = "spearman")

celfvoc_propi_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$propi,
                           method = "spearman")

celfvoc_propn_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$propn,
                           method = "spearman")

celfvoc_propr_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$propr,
                           method = "spearman")

celfvoc_mean_awc_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$mean_awc_perhr,
                              method = "spearman")

celfvoc_sf5_awc_cor <- cor.test(supplement_data$celf_vocab_scaled_score, supplement_data$sf5_awc_perhr,
                             method = "spearman")

# CELF vocabulary model
mod_input_voc <- lm(data = supplement_data, celf_vocab_scaled_score ~ propi)

# TPVT correlations
tpvt_tokens_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$mean_tokens,
                               method = "spearman")

tpvt_types_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$mean_types,
                              method = "spearman")

tpvt_op_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$prop_op,
                           method = "spearman")

tpvt_propi_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$propi,
                              method = "spearman")

tpvt_propn_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$propn,
                              method = "spearman")

tpvt_propr_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$propr,
                              method = "spearman")

tpvt_mean_awc_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$mean_awc_perhr,
                                 method = "spearman")

tpvt_sf5_awc_cor <- cor.test(supplement_data$sf5_pvt_AgeCorrectedScore, supplement_data$sf5_awc_perhr,
                                method = "spearman")

# TPVT model
mod_data_tpvt <- supplement_data %>%
  drop_na(mean_tokens, mean_types, prop_op)

mod_tpvt_big0 <- lm(data = mod_data_tpvt, sf5_pvt_AgeCorrectedScore ~
                     mean_tokens + mean_types + prop_op)
# vif(mod_tpvt_big0)
# Removing tokens due to multicollinearity

mod_tpvt_big <- lm(data = mod_data_tpvt, sf5_pvt_AgeCorrectedScore ~
                     mean_types + prop_op)

step <- MASS::stepAIC(mod_tpvt_big, direction = "backward")

tpvt_best_mod <- lm(sf5_pvt_AgeCorrectedScore ~ mean_types,
                    data = supplement_data)
