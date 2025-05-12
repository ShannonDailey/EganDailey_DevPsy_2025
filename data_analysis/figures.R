# This script generates the two figures in this manuscript.

library(tidyverse)
library(ggpubr)
#devtools::install_github("JLSteenwyk/ggpubfigs")
library(ggpubfigs)

data <- read_csv("data/seedlings_data.csv")

#### FIGURE 1: LANGUAGE SKILLS PLOT ####

celf_predictor_data <- data %>% dplyr::select(subj, cdi_12mo, cdi_18mo,
                                                   sf3_pvt_AgeCorrectedScore, quils_overall_standard,
                                                   celf_core_lang) %>%
  pivot_longer(cols = c(cdi_12mo, cdi_18mo,
                        sf3_pvt_AgeCorrectedScore, quils_overall_standard),
               names_to = "measure",
               values_to = "score") %>%
  mutate(age = as_factor(case_when(measure == "cdi_12mo" ~ "1;0",
                                   measure == "cdi_18mo" ~ "1;6",
                                   measure == "sf3_pvt_AgeCorrectedScore" ~ "3;6",
                                   measure == "quils_overall_standard" ~ "3;6"))) %>%
  filter(!is.na(celf_core_lang))

figure1 <- ggplot(data = celf_predictor_data, aes(x = score, y = celf_core_lang,
                                                           color = measure))+
  geom_point(shape = 1, size = 2)+
  scale_color_manual(values = friendly_pal("nickel_five"))+
  stat_smooth(method = "lm", se = TRUE)+
  stat_cor(method = "spearman", cor.coef.name = "rho", label.y = 145, size = 4.5)+
  facet_wrap(~measure, scales = "free_x", nrow = 3,
             labeller = labeller(measure=c("cdi_12mo" = "MCDI vocabulary @ 1;0",
                                           "cdi_18mo" = "MCDI vocabulary @ 1;6",
                                           "sf3_pvt_AgeCorrectedScore" = "TPVT vocabulary @ 3;6",
                                           "quils_overall_standard" = "QUILS language @ 3;6")))+
  labs(y = "CELF core language skill @ 4;6", x = "Score")+
  scale_y_continuous(limits = c(NA, 150))+
  theme_bw(base_size = 12)+
  guides(color = "none")

####  FIGURE 2: INPUT PLOT ####

lena_predictor_data <- data %>%
  dplyr::select(subj, celf_core_lang,
                mean_awc_perhr,
                # mean_ctc_perhr ,
                sf5_awc_perhr
                # sf5_ctc_perhr
                ) %>% 
  pivot_longer(cols = c(mean_awc_perhr,
                        # mean_ctc_perhr,
                        sf5_awc_perhr
                        # sf5_ctc_perhr
                        ),
               names_to = "input_metric",
               values_to = "value")

noun_predictor_data <- data %>%
  dplyr::select(subj, celf_core_lang,
                mean_tokens, mean_types,
                prop_op, propi, propn, propr) %>% 
  pivot_longer(cols = c(mean_tokens, mean_types,
                        prop_op, propi, propn, propr),
               names_to = "input_metric",
               values_to = "value")

lena_input_plot <- ggplot(data = lena_predictor_data, aes(x = value, y = celf_core_lang,
                                                          color = input_metric))+
  geom_point(shape = 1, size = 1.5, alpha = 0.5)+
  scale_color_manual(values = friendly_pal("vibrant_seven"))+
  stat_smooth(method = "lm", se = TRUE)+
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.y = 145, size = 3,
           r.accuracy = .01)+
  facet_wrap(~input_metric, scales = "free",
             nrow = 2,
             labeller = labeller(input_metric = c("mean_awc_perhr" = "Mean adult words/hr \n (LENA) @ 0;6-1;5",
                                                  # "mean_ctc_perhr" = "Mean convo turns/hr (LENA) @ 0;6-1;5",
                                                  "sf5_awc_perhr" = "Adult words/hr \n (LENA) @ 4;6"
                                                  # "sf5_ctc_perhr" = "Convo turns/hr (LENA) @ 4;6"
                                                  )))+
  scale_x_continuous(limits = c(300, 3200),
                     breaks = c(1000, 2000, 3000))+
  scale_y_continuous(limits = c(NA, 150))+
  labs(#y = "CELF core language skill @ 4;6",
    y = NULL,
       x = "LENA metric")+
  theme_bw(base_size = 10)+
  guides(color = "none")

noun_input_plot <- ggplot(data = noun_predictor_data, aes(x = value, y = celf_core_lang,
                                                          color = input_metric))+
  geom_point(shape = 1, size = 1.5, alpha = 0.5)+
  scale_color_manual(values = friendly_pal("ito_seven"))+
  stat_smooth(method = "lm", se = TRUE)+
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.y = 145, size = 3,
           r.accuracy = .01)+
  facet_wrap(~input_metric, scales = "free_x",
             labeller = labeller(input_metric = c("mean_tokens" = "Word tokens",
                                                  "mean_types" = "Word types",
                                                  "prop_op" = "Prop. object \n presence",
                                                  "propi" = "Prop. imperative \n utterances",
                                                  "propn" = "Prop. short phrases",
                                                  "propr" = "Prop. reading \n utterances")))+
  scale_x_continuous(n.breaks = 4)+
  scale_y_continuous(limits = c(NA, 150))+
  labs(y = "CELF core language skill @ 4;6",
       x = "Noun input metric (mean 0;6-1;5)")+
  theme_bw(base_size = 10)+
  guides(color = "none")

figure2 <- ggarrange(noun_input_plot, lena_input_plot,
                            ncol = 2, nrow = 1, widths = c(1, 0.36))
