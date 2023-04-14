#Visualizing metadata (ie location of eDNA + differences in sampling depth)
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

metatrawl<- select(metatrawl, c('set_number','depth_mean'))

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
metaeDNA$set_number <- as.numeric(metaeDNA$set_number)
metaeDNA <- metaeDNA %>% drop_na(set_number)
metaeDNA <- metaeDNA[!is.na(metaeDNA$depth),]

#merge files 
meta <- merge(metatrawl, metaeDNA, by=c('set_number'))

#take mean depth of eDNA sample per trawl 
meta_new <- meta %>%
  group_by(set_number) %>%
  dplyr::summarise(eDNA_mean_depth = mean(depth))

meta_all <- merge(meta, meta_new, by=c('set_number'))
meta_all <- select(meta_all, c('set_number','depth_mean','eDNA_mean_depth')) #select relevant columns
meta_all <- distinct(meta_all)


#explore depths of trawl and eDNA samples
#give sites different levels based on increasing difference in sample depths 
meta_all$set_number <- factor(meta_all$set_number,levels = c("1", "2", "5", "10",'11','12','3','9','4','7','8','16','14','13','15','6'))

plot <- meta_all %>%
  ggplot(aes(x=factor(set_number), y=eDNA_mean_depth, col="eDNA")) + 
  geom_jitter(width=.15) +
  geom_point(aes(y=depth_mean, col="trawl"), size=1.5) +
  scale_color_manual(values = c("#FCC442","#5491cf")) +
  scale_y_reverse() + theme_bw() +
  labs(y="depth (m)", x="site")  + 
  theme(legend.title= element_blank())

plot

ggsave("./Outputs/metadata/samplingdepths_all.png", 
       plot = plot,
       width = 10, height = 5, units = "in")


#Map of study site ####
#https://jtr13.github.io/cc19/using-stamen-maps-for-plotting-spatial-data.html

#read data 
metatrawl <- read.csv(here::here("Processed_data", 
                                 "trawl",
                                 "metadata", 
                                 "clean_data",
                                 "trawl_metadata.csv"),
                      head=TRUE)


metaeDNA <- read.csv(here::here("Processed_data", 
                                "eDNA",
                                "metadata", 
                                "clean_data",
                                "eDNA_metadata.csv"),
                     head=TRUE)

map <- merge(metatrawl, metaeDNA, by=c('set_number'))
map <- subset(map, !is.na(depth))
map <- select(map, c('depth', 'lat_door_in_dd','long_door_in_dd', 'set_number'))
map <- distinct(map)
map <- subset(map, depth != 5)


register_google('AIzaSyDuNPlxDnIWmspqLybGLH3d30T-_a0rQ-Y') #register API 

#plot in color
map.for.samples <- get_map(location = c(-128,47.5,-122,51.5),
                           maptype = 'terrain', #change this to terrain for terrain background
                           source = 'google',
                           api_key = 'AIzaSyDuNPlxDnIWmspqLybGLH3d30T-_a0rQ-Y') # <- REPLACE WITH YOUR KEY


edna_sample_map<-ggmap(map.for.samples) +
  geom_point(data = map,
             aes(x = long_door_in_dd, y = lat_door_in_dd,
                 colour=depth), size=2) +
  scale_colour_gradient(name='depth of sample', low = '#00AFBB', high = '#006a71') #set colours so dark is deeper

edna_sample_map

#plot in b&w
map.for.samples <- get_map(location = c(-128,47.5,-122,51.5),
                           maptype = 'toner-lite', #change this to terrain for terrain background
                           source = c("stamen"),
                           api_key = 'AIzaSyDuNPlxDnIWmspqLybGLH3d30T-_a0rQ-Y') # <- REPLACE WITH YOUR KEY


edna_sample_map<-ggmap(map.for.samples) +
  geom_point(data = map,
             aes(x = long_door_in_dd, y = lat_door_in_dd,
                 colour=depth), size=2) +
  scale_colour_gradient(name='depth of sample', low = '#00AFBB', high = '#006a71') + #set colours so dark is deeper
  ylab(c("latitude")) + xlab(c("longitude")) 

edna_sample_map

ggsave("./Outputs/metadata/mapbw.png", 
       plot = edna_sample_map,
       width = 5, height = 5, units = "in")


