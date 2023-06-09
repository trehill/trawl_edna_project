#Visualizing metadata- sampling depth 
#Author: Tessa Rehill 

#Set-up ####
library(tidyr)
library(tidyverse)
library(ggplot2)
library(here)
library(maps)
library(dplyr)
library(ggmap)


#function
#this function calculates degrees from decimal coordinates 
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

#read files 
metatrawl <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "metadata", 
                            "clean_data",
                            "trawl_metadata.csv"),
                 head=TRUE)

metatrawl<- select(metatrawl, c('set_number','depth_mean', 'bottom_depth_m'))

metaeDNA <- read.csv(here::here("Processed_data", 
                                 "eDNA",
                                 "metadata", 
                                 "clean_data",
                                 "eDNA_metadata.csv"),
                      head=TRUE)

latlong <- read.csv(here::here("Processed_data", 
                               "trawl",
                               "metadata", 
                               "clean_data",
                               "lat_lon_all.csv"),
                    head=TRUE)

#Compare depth of sampling between methods ####
metaeDNA <- select(metaeDNA, c('set_number', 'depth', 'lat','lon'))

#change set_number to integer
metaeDNA$set_number <- as.numeric(metaeDNA$set_number) #this is because TS1 and TS2 are in the df (irrelevant)
metaeDNA <- metaeDNA %>% drop_na(set_number)
metaeDNA <- metaeDNA[!is.na(metaeDNA$depth),]

#merge files 
meta <- merge(metatrawl, metaeDNA, by=c('set_number'))

#take mean depth of eDNA sample per trawl 
meta_new <- meta %>%
  group_by(set_number) %>%
  dplyr::summarise(eDNA_mean_depth = mean(depth))

meta_all <- merge(meta, meta_new, by=c('set_number'))

#add new column for difference in depth 
meta_all$depth_difference <- abs(meta_all$depth_mean - meta_all$eDNA_mean_depth)
#order of increasing depth difference: #1,2,5,10,12,3,9,13,4,7,8,16,14,11,15,6

meta_all <- select(meta_all, c('set_number','depth_mean','eDNA_mean_depth', 'bottom_depth_m')) #select relevant columns
meta_all <- distinct(meta_all)

#explore depths of trawl and eDNA samples
#give sites different levels based on increasing difference in sample depths 

meta_all$set_number <- factor(meta_all$set_number,levels = 
  c("1", "2", "5", "10",'12','3','9','13','4','7','8','16','14','11','15','6'))

plot <- meta_all %>%
  ggplot(aes(x=factor(set_number), y=eDNA_mean_depth, col="eDNA")) + 
  geom_jitter(width=.17) +
  geom_point(aes(y=depth_mean, col="trawl"), size=1.5) +
  geom_point(aes(y=bottom_depth_m, col="bottom depth"), size=1.0)+
  scale_color_manual(values = c("#031618","#FCC442","#5491cf")) +
  scale_y_reverse() + theme_bw() +
  labs(y="depth (m)", x="site")  + 
  theme(legend.title= element_blank())

plot

ggsave("./Outputs/metadata/samplingdepths_all.png", 
       plot = plot,
       width = 10, height = 5, units = "in")
