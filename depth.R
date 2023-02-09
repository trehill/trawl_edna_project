#DEPTH 
#Exploring depth difference of sampling as a driver for difference in community structure

#Set-Up ####
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)

# if this code works - delete from traits code 
# need to add 'save' graph codes


#set-up, read in files ####

beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "beta_div.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)

trawl_catch  <- read.csv(here::here("Processed_data", 
                                    "trawl",
                                    "catch_data",
                                    "clean_data",
                                    "trawl_catch.csv"),
                         head=TRUE)

trawl_meta <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

eDNA_meta <- read.csv(here::here("Processed_data", 
                                 "eDNA",
                                 "metadata",
                                 "clean_data",
                                 "eDNA_metadata.csv"),
                      head=TRUE)

div_ind <- read.csv(here::here("Processed_data", 
                               "datasets",
                               "diversity_indices.csv"),
                    head=TRUE)

#DEPTH 

#determine depth difference between trawl + eDNA sampling 


#DEPTH 

#need to make difference between trawl + eDNA sampling depth + Jaccards indices 

eDNA_meta <- select(eDNA_meta, c('set_number', 'depth'))
eDNA_meta <- subset(eDNA_meta, depth != 5) #exclude 5m 
eDNA_meta <- eDNA_meta %>% drop_na(depth)
eDNA_meta <- eDNA_meta %>% rename(depth_eDNA = depth,)
eDNA_meta <- distinct(eDNA_meta)

trawl_meta <- select(trawl_meta, c('set_number', 'depth_mean'))
trawl_meta <- trawl_meta %>% rename(depth_trawl = depth_mean,)
trawl_meta <- distinct(trawl_meta)

depth_set <- merge(trawl_meta, eDNA_meta, by=c('set_number'))

#linear regression beta div (jaccards index) over depth difference ####
depth_beta <- merge(depth_set, div_ind, by=c('set_number'))
depth_beta <- depth_beta %>% drop_na(depth_eDNA)
depth_beta <- distinct(depth_beta)

#add new column for difference in depth 
depth_beta$depth_difference <- abs(depth_beta$depth_eDNA - depth_beta$depth_trawl)

#plot 
#jaccards index over depth difference 

ggplot(depth_beta,aes(depth_difference, Jac)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Jaccards Index (beta diversity)',title='Beta Diversity / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()

#nestedness over depth difference 

ggplot(depth_beta,aes(depth_difference, Jac_nest)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Jaccards Nestedness Index',title='Nestedness / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()

#turnover over depth difference 
ggplot(depth_beta,aes(depth_difference, Jac_turn)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Jaccards Turnover Index',title='Turnover / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()

#shared species over depth difference 
shared <- merge(beta_div, depth_beta, by=c('set_number'))
x <- shared

df2 <- x %>% group_by(set_number, alpha_detection_method) %>% 
  summarise(shared_count=n(),.groups = 'drop')

df3 <- df2 %>% group_by(set_number) %>% 
  summarise(total_count = sum(shared_count))

df4 <- merge(df2, df3, by=c('set_number'))

#subset for method = both 
df5 <- subset(df4, alpha_detection_method == c('both eDNA/trawl'))
df5$shared_over_total <- abs(df5$shared_count/df5$total_count)

#combine to depth difference 

shared_depth <- merge(depth_beta, df5, by=c('set_number'))

ggplot(shared_depth,aes(depth_difference, shared_over_total)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Number Shared Species / Total Number Species',title='Shared Species / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()