library(tidyverse)
library(here)

#This project has two raw data files at different scales from a study of
#infants, children, and adults watching a series of 7 video clips

#I wrote Steps 1 and 2 to import and merge the data, and kept them here for your reference
#Skip down to Step 3 to work on EDA

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
auc <- read_csv(here("data_raw", "auc_bystim.csv"))

stim_levels <- 1:7
stim_labels <- c("Fallon","Feist","Pentatonix","Science","Rube","Plane","Dogs")
auc <- auc %>% mutate(stim = factor(stim, levels = stim_levels, labels = stim_labels))
fct_count(auc$stim)


#STEP 2: READ IN THE PPT INFO DATA
#Wrangle the ppt info data so that you can merge it into the auc data
#Drop any data where the AUC values are missing
#In the final, merged data, make the "watched" variable is coded as a factor with 
#  levels "seen" (1), "not seen" (2), "not sure" (3) 
#Write the cleaned file to data_cleaned/ 

#Read in the ppt data and rename columns to be easier to work with
ppt <- read_csv(here("data_raw","participants_info_full_headers.csv")) %>% 
  rename(id = `participant ID`,
         age_group = `Age group`,
         precision = "Precision")

#Each question about watching each video is a column, so pivot_longer
#Use separate to get just the video name into it's own column
ppt_long <- ppt %>% pivot_longer(cols = starts_with("Seen"), names_to = "stim", values_to = "watched")
ppt_long <- ppt_long %>% separate(stim, into = c(NA, "stim"))

#Code stim and watched as factors
ppt_long <- ppt_long %>% mutate(
  stim = factor(stim, levels = stim_labels, labels = stim_labels),
  watched = factor(watched, levels = 1:3, labels = c("Yes","No","Not Sure")))

#Join the ppt data to the AUC data (by id and by stim since each participant has observations for each stim)
ds <- left_join(auc, ppt_long, by = c("id", "stim"))
ds <- ds %>% drop_na(AUC_sal:AUC_dist) #Drop participants for whom we don't have data for the DV

#Write the data to file
ds %>% write_csv(here("data_cleaned","cleaned.csv"))

#STEP 3: EXPLORATORY DATA ANALYSIS

#3A PRECISION: Is the precision acceptable (< 2.5) for each participant?
  #Are data equally precise for participants of different ages?
  #Which participants would we need to exclude if > 2.5 meant the data are unuseable?

ds %>% ggplot() + geom_histogram(aes(x = precision)) + geom_vline(xintercept = 2.5)
ds %>% ggplot() + geom_point(aes(x = age_years, y = precision)) + geom_hline(yintercept = 2.5)
ds %>% ggplot() + geom_boxplot(aes(x = age_group, y = precision)) + geom_hline(yintercept = 2.5)

ds %>% group_by(age_group) %>% summarize(across(precision, list(M = mean, MIN = min, MAX = max)))

ds %>% group_by(id, age_group) %>% summarize(precision = mean(precision, na.RM = T)) %>% filter(precision > 2.5)

#3B AGE: Are there any errors in age? 
#Convert age to years so that it can be more easily compared to age_group
#How can you visualize age in years and how it relates to age_group categories?
#Are all of the ages in each group correct (i.e., within the bounds)?

#Convert age in days to age in years
ds <- ds %>% mutate(age_years = age/365.25)

#A boxplot of age by age group can help find severe outliers (like the almost 4-year-old who got put in the 1.5-2 group)
ds %>% group_by(id, age_group) %>% 
  summarize(age_years = mean(age_years)) %>% 
  ggplot(aes(y = age_group, x = age_years)) + geom_boxplot()

#Another options would be to facet by age group and to let the scales be "free" to get a better look
ds %>% group_by(id, age_group) %>% 
  summarize(age_years = mean(age_years)) %>% 
  ggplot(aes(y = age_years)) + 
  geom_boxplot() + 
  facet_wrap("age_group", scales = "free")

#In this case, there are some close borderline cases they we're not able to see in the graphs
#Checking mins and maxes does a better job
ds %>% group_by(age_group) %>% summarize(min_age = min(age_years), max_age = max(age_years))

#The code above works well enough, but if we wanted to be really exact we could turn age group
# into a min/max range and then use it to make logical checks
# (case_when is an ifelse variant for when you have more than 2 options)
# I'm frustrated with how long this code took to write, so I'm curious if anyone has a better way to do it!
ds <- ds %>% mutate(
  age_group_min = case_when(
    age_group == ".5-1 y" ~ .5,
    age_group == "1-1.5 y" ~ 1,
    age_group == "1.5-2 y" ~ 1.5,
    age_group == "2-4 y" ~ 2,
    age_group == "4-6 y" ~ 4,
    age_group == "8-10 y" ~ 8,
    age_group == "adult" ~ 18),
  age_group_max = case_when(
    age_group == ".5-1 y" ~ 1,
    age_group == "1-1.5 y" ~ 1.5,
    age_group == "1.5-2 y" ~ 2,
    age_group == "2-4 y" ~ 4,
    age_group == "4-6 y" ~ 6,
    age_group == "8-10 y" ~ 10,
    age_group == "adult" ~ 30)
)
#With our new min/max variables, now we can directly check age against the possible values within the group
ds %>% group_by(id, age_years, age_group_min, age_group_max) %>% 
  filter(age_years < age_group_min | age_years > age_group_max) %>% 
  summarize(id = mean(id)) %>% arrange(age_years)

#3C SEEN VIDEOS BEFORE:
  #How many participants in each age group have seen the videos before?
  #How many total participants saw each video before? 

#Summing "watched == "Yes" will give us the summary of how many ppts in each grouping have watched each video
#Pivoting wider makes it a little bit more readable, and also makes it easier for us to do an additional
#summary across age groups
ds %>% group_by(age_group, stim) %>% 
  summarize(n_watched = sum(watched == "Yes")) %>% 
  pivot_wider(id_cols = "age_group", names_from = "stim", values_from = "n_watched") %>% 
  print %>% ungroup %>% summarize(across(Feist:Dogs, sum))

ds %>% ggplot(aes(x = watched)) + geom_bar() + facet_grid("stim")

#3D AUC VALUES:
  #Are the two AUC values all within the possible range (0,1)? 
  #Does AUC seem to differ according to stim and/or age?

#Easier if we pivot AUC to longer
ds_longer <- ds %>% pivot_longer(starts_with("AUC"), names_to = "model", values_to = "AUC")

#Histogram of AUC with bar fills determined by model, seems like they're all in range
ds_longer %>% ggplot(aes(x = AUC, fill = model)) + geom_histogram() + xlim(0,1)

#Summary table version shows that all AUCs are > 0 and < 1
ds_longer %>% group_by(model) %>% summarize(min = min(AUC), max = max(AUC))

#Plot AUC by age...doesn't really look like much is going on
ds_longer %>% ggplot(aes(x = age, y = AUC, color = model)) + geom_point()

#Let's check boxplots by stim and model - maybe a hint
ds_longer %>% ggplot(aes(x = stim, y = AUC, fill = model)) + geom_boxplot()

#Plot AUC by age and panel by stim
ds_longer %>% ggplot(aes(x = age, y = AUC, color = model)) + 
  geom_point() + 
  facet_wrap(~stim)
  xlim(0,1)
  




  
  

