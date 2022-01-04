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
  #Visualize the distribution of precision to see if there are values above 2.5
  #Create a summary to figure out which participants would we need to exclude if > 2.5 meant the data are unuseable? 
  #Use a summary table and plots to investigate whether data equally precise for participants of different ages


#3B AGE: Are there any errors in age? 
#Convert age to years so that it can be more easily compared to age_group
#Visualize age in years by age_group to see whether participants are the correct age for their group
#Make a summary table of age in years by age group to check whether all participants' ages are correct


#3C SEEN VIDEOS BEFORE:
  #How many total participants saw each video before collapsed across age? Make a summary table and a bar plot to illustrate this.
  #Make a table to show how many participants in each age group have seen the videos before



#3D AUC VALUES:
  #Are the two AUC values all within the possible range (0,1)? Create a plot and a summary table to investigate.
  #Plot AUCs by stimulus and age to explore whether they might be related. 
    #How do they compare to chance (.5)? Plot AUCs across their full range 0-1.





  
  

