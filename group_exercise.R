library(tidyverse)
library(here)

auc <- read_csv(here("data_raw", "auc_bystim.csv"))

stim_levels <- 1:7
stim_labels <- c("Fallon","Feist","Pentatonix","Science","Rube","Plane","Dogs")

auc <- auc %>% mutate(stim = factor(stim, levels = stim_levels, labels = stim_labels))

fct_count(auc$stim)

ppt <- read_csv(here("data_raw","participants_info_full_headers.csv")) %>% 
  rename(id = `participant ID`,
         age_group = `Age group`,
         precision = "Precision")

ppt_long <- ppt %>% pivot_longer(cols = starts_with("Prior to today"), names_to = "stim", values_to = "watched")

ppt_long <- ppt_long %>% mutate(
  stim = str_remove(stim, fixed("Prior to today, have you seen this video before? ")),
  stim = str_remove(stim, fixed(" (1=yes, 2=no, 3=not sure)")),
  stim = str_trim(stim))

ppt_long <- ppt_long %>% mutate(
  stim = factor(stim, levels = stim_labels, labels = stim_labels),
  watched = factor(watched, levels = 1:3, labels = c("Yes","No","Not Sure")))

ds <- left_join(auc, ppt_long, by = c("id", "stim"))

ds <- ds %>% mutate(age_years = age/365.25)
ds <- ds %>% drop_na(AUC_sal:AUC_dist)

ds %>% write_csv(here("data_cleaned","cleaned.csv"))


ds %>% ggplot() + geom_histogram(mapping = aes(x = age_years))

ds %>% ggplot() + geom_histogram(mapping = aes(x = precision))
ds %>% ggplot() + geom_point(mapping = aes(x = age_years, y = precision))

ds %>% group_by(age_group) %>% summarise(min_age = min(age_years), max_age = max(age_years))

ds %>% group_by(age_group, stim) %>% summarize(n_watched = sum(watched == "Yes")) %>% 
  pivot_wider(id_cols = "age_group", names_from = "stim", values_from = "n_watched")
