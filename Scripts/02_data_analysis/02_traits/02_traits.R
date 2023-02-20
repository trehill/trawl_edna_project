#Trait Exploration New

#Set-Up ####
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)

#set-up, read in files ####

beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "detections.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)


trawl_catch  <- read.csv(here::here("Processed_data", 
                                    "trawl",
                                    "catch_data",
                                    "clean_data",
                                    "trawl_catch_clean.csv"),
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
                            "diversity",
                            "diversity_indices.csv"),
                 head=TRUE)



#create df, named data  ####

#merge identification data w/ trait data 
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)

#change 'both' to eDNA or trawl alone

#subset for 'both' data only 
both_to_eDNA <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to eDNA 
both_to_eDNA <- data.frame(lapply(both_to_eDNA, function(x) {
  gsub("both eDNA/trawl", "eDNA", x) 
  
}))

#same for trawl 
both_to_trawl <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to trawl 
both_to_trawl <- data.frame(lapply(both_to_trawl, function(x) {
  gsub("both eDNA/trawl", "trawl", x) 
  
}))

#merge together 
both_fixed <- rbind(both_to_eDNA, both_to_trawl)

#merge back with our original data
without_both <- subset(data, gamma_detection_method != 'both eDNA/trawl')
data_new <- rbind(both_fixed, without_both)

#fix only... to simple trawl or eDNA 
data <- data.frame(lapply(data_new, function(x) {
  gsub("only eDNA", "eDNA", x) 
  
}))

data <- data.frame(lapply(data, function(x) {
  gsub("only trawl", "trawl", x) 
  
}))


#let's plot!

#length ####

#check out ind. length info 
trawl_catch <- trawl_catch[!is.na(trawl_catch$length_cm),] #remove NA

#subset out 'fork length' we only want total length
#i don't know why we also have 'fork' length
#trawl_catch <- trawl_catch[trawl_catch$length_type == 'Total',]

plot <- ggplot(trawl_catch, aes(x=length_cm))+
  geom_histogram(color="lightblue", fill="lightblue", binwidth = 2,)+
  theme_classic()
plot


ggsave("./Outputs/traits/ind_length_his.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#let's just look at species 

speciestraits= subset(data, select = c(LCT, max_length_cm, gamma_detection_method) )
speciestraits <- distinct(speciestraits)
speciestraits <- speciestraits %>% drop_na(max_length_cm)

#species length distributions 
speciestraits$max_length_cm <- as.numeric(speciestraits$max_length_cm) 

plot <- ggplot(speciestraits, aes(max_length_cm)) +
  geom_density(aes(fill = gamma_detection_method), position = "stack") + 
  theme_classic()

plot

ggsave("./Outputs/traits/spp_length_dens.png", 
       plot = plot,
       width = 20, height = 12, units = "in")

write_csv(speciestraits,
          here("Processed_data",
               "traits",
               "length_2.csv")) 

#another plot 
plot <- ggplot(speciestraits, 
       aes(x = max_length_cm, 
           y = gamma_detection_method, 
           fill = gamma_detection_method)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("") +
  theme(legend.position = "none") +
  theme_classic()
plot 

ggsave("./Outputs/traits/spp_length_dens2.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#goal: make a density plot like above with four rows 
#trawl only max species length 
#eDNA only max species length 
#trawl + eDNA max species length 
#trawl only individual length 

#merge original data w/ traits 
data2 <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)
#extract relevant columns 
data2 <- select(data2, c('LCT','gamma_detection_method', 'max_length_cm'))
colnames(data2) <- c('LCT', 'detection', 'length_cm')

catch <- select(trawl_catch, c('species', 'length_cm')) 
#change column names
colnames(catch) <- c('LCT','length_cm')
#add column detection = trawl individuals

catch$detection <- c('trawl individuals')

#merge all data

length <- rbind(catch, data2) #this includes non-fish... should we get rid of them? 

write_csv(length,
          here("Processed_data",
               "traits",
               "length.csv")) 
#plot
plot <- ggplot(length, 
               aes(x = length_cm, 
                   y = detection, 
                   fill = detection)) +
  geom_density_ridges(bandwidth=5) + 
  theme_ridges() +
  labs("") +
  theme(legend.position = "none") +
  theme_classic()
plot 

ggsave("./Outputs/traits/length.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#habitat ####

#check out environment 
data1 <- subset(data, !is.na(habitat))
data1 <- select(data1, c('gamma_detection_method', 'habitat'))
data1 <-  data1 %>% 
  rename(method = gamma_detection_method)

data1$observation_count <- 1 

#specify order of bars (from top to bottom)
data1$habitat <- factor(data1$habitat, levels=c('demersal', 'bathydemersal', 'bathypelagic','pelagic', 'benthopelagic','reef_associated' ))

#create stacked bar chart
plot <- ggplot(data1, aes(x=habitat, y=observation_count, fill=method)) + 
  geom_bar(position='stack', stat='identity')+
  theme_classic()
plot
  

ggsave("./Outputs/traits/habitat_hist.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


