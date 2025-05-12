# This script generates the figure in the Supplementary Materials.

supplement_data <- read_csv("data/supplement/supplement_data.csv")

#### FIGURE S2 ####

ed_order <- c("less_than_bachelors", "bachelors", "more_than_bachelors")

celf_edu_categorical <- ggplot(data = supplement_data, aes(x = factor(mat_ed, ed_order), y = celf_core_lang)) +
  geom_jitter(aes(shape=mat_ed), width = .1, height = 0,
              size = 2) +
  stat_summary(fun.data = "mean_cl_boot", color="turquoise")+
  labs(y = "CELF core language skill @ 4;6", x = "Maternal education level")+
  theme_bw(base_size = 9)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  scale_x_discrete(labels = c("Less than \n Bachelor's degree",
                              "Bachelor's degree",
                              "More than \n Bachelor's degree"))+
  scale_shape_discrete(name = "Education level",
                       labels = c("Less than Bachelor's degree",
                                  "Bachelor's degree",
                                  "More than Bachelor's degree"))

celf_edu_continuous <- ggplot(data = supplement_data %>% mutate(mat_ed = factor(mat_ed, ed_order)),
                              aes(x = mat_ed_yrs, y = celf_core_lang)) +
  geom_jitter(aes(shape=mat_ed), width = .1, height = 0,
              size = 2) +
  labs(y = "CELF core language skill @ 4;6",
       x = "Maternal education (years)")+
  stat_smooth(method = "lm", se = TRUE, color = "turquoise")+
  theme_bw(base_size = 9)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  scale_shape_discrete(name = "Education level",
                       labels = c("Less than Bachelor's degree",
                                  "Bachelor's degree",
                                  "More than Bachelor's degree"))

figure_s2 <- ggarrange(celf_edu_categorical, celf_edu_continuous,
                           common.legend = TRUE, legend = "right",
                           align = "h")
