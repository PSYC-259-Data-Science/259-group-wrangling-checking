library(tidyverse)
library(here)

#This project has two raw data files at different scales from a study of
#infants, children, and adults watching a series of 7 video clips

#FILE 1: auc.csv
#Columns =  stim (stimulus video, levels/labels provided below)
#           id (unique participant identifier)
#           age (in days)
#           AUC_sal (area-under-the-curve for a saliency model)
#           AUC_dist (area-under-the-curve for a distance model)
#AUC values indicate how well each model predicted where participants looked 
#when watching a video
#AUC values can range from 0-1 where .5 is chance and 1 is perfect prediction

#FILE 2: participants_info_full_headers.csv
#Columns =  id (unique participant identifier, matches auc.csv)
#           age_group (a categorical age variable with levels:
#             ".5-1 y" "1-1.5 y" "1.5-2 y" "2-4 y" "4-6 y" "8-10 y" "adult" 
#           precision (a quality measure of the eye data, smaller is better)
#           7 columns of "Seen X" the stimulus video before the study
#             coded as SEEN (1), NOT SEEN (2), NOT SURE (3) 


#STEP 1: READ IN THE AUC DATA AND CODE STIM AS A FACTOR 
#I will provide the stim_levels and labels here

auc <- read_csv(here("data_raw", "auc_bystim.csv"))

stim_levels <- 1:7
stim_labels <- c("Fallon","Feist","Pentatonix","Science","Rube","Plane","Dogs")

auc <- auc %>% mutate(stim = factor(stim, levels = stim_levels, labels = stim_labels))

fct_count(auc$stim)

#STEP 2: READ IN THE PPT INFO DATA
#Wrangle the ppt info data so that you can merge it into the auc data
#Your final, merged data should look like the example in "data_cleaned/"
#Convert age to years so that it can be more easily compared to age_group
#Drop any data where the AUC values are missing
#Write the cleaned file to data_cleaned/

ppt <- read_csv(here("data_raw","participants_info_full_headers.csv")) %>% 
  rename(id = `participant ID`,
         age_group = `Age group`,
         precision = "Precision")

ppt_long <- ppt %>% pivot_longer(cols = starts_with("Seen"), names_to = "stim", values_to = "watched")

ppt_long <- ppt_long %>% separate(stim, into = c(NA, "stim"))

ppt_long <- ppt_long %>% mutate(
  stim = factor(stim, levels = stim_labels, labels = stim_labels),
  watched = factor(watched, levels = 1:3, labels = c("Yes","No","Not Sure")))

ds <- left_join(auc, ppt_long, by = c("id", "stim"))

ds <- ds %>% mutate(age_years = age/365.25)
ds <- ds %>% drop_na(AUC_sal:AUC_dist)

ds %>% write_csv(here("data_cleaned","cleaned.csv"))

#STEP 3: DATA EXPLORATION

#3A AGE: Are there any errors in age?
  #How can you visualize age in years and how it relates to age_group categories?
  #Are all of the ages in each group correct (i.e., within the bounds)?

ds %>% group_by(id, age_group) %>% summarize(age_years = mean(age_years)) %>% 
  ggplot(mapping = aes(x = age_years, fill = age_group)) + geom_histogram(binwidth = .5)

ds %>% group_by(id, age_group) %>% summarize(age_years = mean(age_years)) %>% 
  ggplot(mapping = aes(y = age_group, x = age_years)) + geom_boxplot()

ds %>% group_by(age_group) %>% summarise(min_age = min(age_years), max_age = max(age_years))


#3B PRECISION: Is the precision acceptable (< 2.5) for each participant?
  #Are data equally precise for participants of different age?

ds %>% ggplot() + geom_histogram(mapping = aes(x = precision)) 
ds %>% ggplot() + geom_point(mapping = aes(x = age_years, y = precision))

ds %>% group_by(age_group) %>% summarize(across(precision, list(M = mean, MIN = min, MAX = max)))

#3C SEEN VIDEOS BEFORE:
  #How many participants in each age group have seen the videos before?
  #How many total participants saw each video before? 

ds %>% group_by(age_group, stim) %>% summarize(n_watched = sum(watched == "Yes")) %>% 
  pivot_wider(id_cols = "age_group", names_from = "stim", values_from = "n_watched") %>% 
  print %>% ungroup %>% summarize(across(Feist:Dogs, sum))

#3D AUC VALUES:
  #Are the two AUC values all within the possible range (0,1)? 
  #Do the two AUCs appear to change with age?
  #Does AUC seem to differ according to video and age?

#Easier if we pivot AUC to longer
ds_longer <- ds %>% pivot_longer(starts_with("AUC"), names_to = "model", values_to = "AUC")

ds_longer %>% ggplot(aes(x = AUC, color = model)) + geom_freqpoly() + xlim(0,1)
ds_longer %>% group_by(model) %>% summarize(min = min(AUC), max = max(AUC))

ds_longer %>% ggplot(aes(x = age, y = AUC, color = model)) + geom_point()+ xlim(0,1)

ds_longer %>% ggplot(aes(x = age, y = AUC, color = model)) + 
  geom_point() + 
  facet_wrap(~stim)
  xlim(0,1)
  




  
  

