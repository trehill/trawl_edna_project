#Explore Alpha Diversity 

#SET-UP ####
#load libraries 
library(eulerr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(here)
library(dplyr)

#read in files 
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

#this kind of worked but we want them to overlap 
data_long <- select(data, c('LCT', 'set_number', 'pabs_trawl', 'pabs_eDNA'))

#we want to change all p_abs_trawl/edna 1 = true, 0 = false
data_long$pabs_trawl<-ifelse(data_long$pabs_trawl=="1",TRUE,FALSE)
data_long$pabs_eDNA<-ifelse(data_long$pabs_eDNA=="1",TRUE,FALSE)

#rename pabs variables 
data_long <- data_long %>% 
  rename(trawl = pabs_trawl, #rename pabs variables 
         eDNA = pabs_eDNA) %>% 
  select(-c('LCT')) #remove LCT

#give set_numbers 'level's in order of increasing sets
data_long$set_number <- as.character(data_long$set_number)
data_long$set_number = factor(data_long$set_number, levels = c('1', '2', '3','4', '5','6',
                                                               '7','8','9','10','11','12',
                                                               '13','14','15','16'))



plot <- plot(euler(data_long, by = list(set_number)), legend = TRUE, fills = c("#5491cf","#FCC442","#00AFBB"))
plot 

ggsave("./Outputs/diversity/alpha_eulerr_all.png", 
       plot = plot,
       width = 12, height = 2, units = "in")



