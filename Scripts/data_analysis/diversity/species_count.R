#All diversity COUNTS
#phylopic imaging of Venn diagrams 

#goal: count how many occurences of each LCT in GAMMA diversity (across all)

#SET UP 
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(here)
library(dplyr)

data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections.csv"),
                 head=TRUE)

#count how many times a species is seen across the dataset 

x <- data

count_spp <- x %>% 
  count(LCT)

#merge w/ big data 

data_count <- merge(x, count_spp, by=c('LCT'))

#xtract only information we want 

df <- select(data_count, c('LCT','n','gamma_detection_method'))
df <- distinct(df)

write_csv(df,
          here("Processed_data",
               "datasets",
               "species_count.csv")) 
