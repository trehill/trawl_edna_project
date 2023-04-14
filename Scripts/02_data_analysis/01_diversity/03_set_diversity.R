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

#select columns of interest 
data2 <- select(data, c('LCT', 'alpha_detection_method', 'set_number'))

#remove duplicates
data2 <- distinct(data2)

#format data to be able to plot (binary T/F to character categories)
data_long <- select(data, c('LCT', 'set_number', 'alpha_detection_method'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, alpha_detection_method, var)
data_wide[is.na(data_wide)] <- FALSE  #replace NA with FALSE 
data_new <- select(data_wide, -c('LCT'))


#this kind of worked but we want them to overlap 
data_long <- select(data, c('LCT', 'set_number', 'pabs_trawl', 'p_abs_eDNA'))

#we want to change all p_abs_trawl/edna 1 = true, 0 = false
data_long$pabs_trawl<-ifelse(data_long$pabs_trawl=="1",TRUE,FALSE)
data_long$p_abs_eDNA<-ifelse(data_long$p_abs_eDNA=="1",TRUE,FALSE)

#rename pabs variables 
data_long <- data_long %>% 
  rename(
    trawl = pabs_trawl,
    eDNA = p_abs_eDNA
  )

data_long <- select(data_long, -c('LCT'))

#arrange in ascending set_number 
data_long <- data_long %>% arrange(as.numeric(set_number)) 

data_long$set_number <- as.character(data_long$set_number)

plot <- plot(euler(data_long, by = list(set_number)), legend = TRUE, fills = c("#5491cf","#FCC442","#00AFBB"))
plot 

ggsave("./Outputs/diversity/alpha_eulerr_all.png", 
       plot = plot,
       width = 12, height = 2, units = "in")

