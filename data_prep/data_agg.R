
# This script creates the public-facing data frame used for visualizations and analyses.
# Shannon Egan-Dailey, shannon.dailey@duke.edu

library(tidyverse)
# install.packages("remotes")
# remotes::install_github('BergelsonLab/blabr')
library(blabr)

##### IMPORT AND PREP LENA DATA #####
LENA_data <- read_csv("data/lena_data.csv")

infancy_lena_summary <- LENA_data %>%
  filter(month != 54) %>% 
  group_by(subj) %>%
  summarise(mean_awc_perhr = mean(awc_perhr),
            mean_ctc_perhr = mean(ctc_perhr),
            mean_cvc_perhr = mean(cvc_perhr))

preschool_lena_summary <- LENA_data %>%
  filter(month == 54) %>%
  dplyr::select(subj, awc_perhr, cvc_perhr, ctc_perhr) %>%
  rename("sf5_awc_perhr" = "awc_perhr",
         "sf5_cvc_perhr" = "cvc_perhr",
         "sf5_ctc_perhr" = "ctc_perhr")

##### IMPORT AND PREP NOUN COUNTS DATA #####

seedlings_nouns <- get_seedlings_nouns('v1.0.0')

all_bl_top3 <- seedlings_nouns %>%
  filter(is_top_3_hours == TRUE | audio_video == "video") %>%
  rename("subj" = "child")

types_tokens <- all_bl_top3 %>%
  filter(speaker != "CHI") %>%
  dplyr::group_by(subj, month) %>%
  summarise(numtokens = n(),
            numtypes = n_distinct(basic_level))

prop_op <- all_bl_top3 %>%
  filter(speaker != "CHI") %>%
  dplyr::filter(object_present %in% c("n","y")) %>%
  group_by(subj, object_present) %>%
  tally() %>%
  spread(object_present, n) %>% 
  summarise(prop_op = y / (n + y))

utt_types <- all_bl_top3 %>%
  filter(speaker != "CHI" & utterance_type %in%c("d","i","q","r","s","n")) %>%
  group_by(subj, utterance_type) %>%
  dplyr::group_by(subj, month, utterance_type) %>%
  dplyr::tally() %>%
  tidyr::spread(utterance_type, n) %>%
  mutate(d = replace_na(d, 0),
         i = replace_na(i, 0),
         n = replace_na(n, 0),
         q = replace_na(q, 0),
         r = replace_na(r, 0),
         s = replace_na(s, 0),
         sum = d+i+n+q+r+s,
         propd = d/sum,
         propi = i/sum,
         propn = n/sum,
         propq = q/sum,
         propr = r/sum,
         props = s/sum) %>%
  group_by(subj) %>%
  summarise(propd = mean(propd),
         propi = mean(propi),
         propn = mean(propn),
         propq = mean(propq),
         propr = mean(propr),
         props = mean(props))

input_nouns <- types_tokens %>%
  group_by(subj) %>%
  summarise(mean_tokens = mean(numtokens),
            mean_types = mean(numtypes)) %>%
  left_join(prop_op) %>%
  left_join(utt_types)

##### IMPORT AND PREP LANGUAGE SKILL DATA #####

all_vocab <- read_csv("data/all_vocab.csv") %>%
  mutate(subj = as.factor(subj))

##### CREATE AGGREGATED DATA FRAME #####

input_vocab_data <- all_vocab %>%
  left_join(infancy_lena_summary) %>%
  left_join(preschool_lena_summary) %>% 
  left_join(input_nouns)

write_csv(input_vocab_data, "data/seedlings_data.csv")
