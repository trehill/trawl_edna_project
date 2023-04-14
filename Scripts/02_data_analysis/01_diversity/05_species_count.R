#Species count
#Author: Tessa Rehill 
#goal: count how many occurences of each LCT in GAMMA diversity (across all)

#SET UP ####
#load libraries
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)

#read in data 
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

#count
x <- data

count_spp <- x %>% 
  count(LCT) #count how many times a species is seen across the dataset 

#merge w/ original detection data 
data_count <- merge(x, count_spp, by=c('LCT'))

#extract only information we want 
df <- select(data_count, c('LCT','n','gamma_detection_method')) #select columns 
df <- distinct(df) #make sure no duplicates

#write file to see! 
write_csv(df,
          here("Processed_data",
               "diversity",
               "species_count_all.csv")) 





