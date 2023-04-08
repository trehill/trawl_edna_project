#Species count

#goal: count how many occurences of each LCT in GAMMA diversity (across all)

#SET UP 
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)



#ALL SETS ####

data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

#count how many times a species is seen across the dataset 

x <- data

count_spp <- x %>% 
  count(LCT)

#merge w/ big data 

data_count <- merge(x, count_spp, by=c('LCT'))

#extract only information we want 

df <- select(data_count, c('LCT','n','gamma_detection_method'))
df <- distinct(df)

write_csv(df,
          here("Processed_data",
               "diversity",
               "species_count_all.csv")) 

#count detections 

x <- df

count_spp <- x %>% 
  count(gamma_detection_method)

