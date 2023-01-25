#Trait Exploration New

#Set-Up ####
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(here)
library(dplyr)

trawl_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  "fulldatasettrawlmin.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "eDNAfulldatasetmin.csv"),
                      head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)

method_key  <- read.csv(here::here("Processed_data", #only applies to gamma level methods
                                    "euler",
                                    "method_key_all.csv"),
                                  head=TRUE)

trawl_catch  <- read.csv(here::here("Processed_data", 
                                   "trawl",
                                   "catch_data",
                                   "clean_data",
                                   "trawl_catch.csv"),
                        head=TRUE)

#we want to add traits to trawl_data, and eDNA_data, as well as metho

#let's start with traits 
#add traits to trawl 
trait_trawl <- merge(trawl_data, trait_data, by.x="LCT", by.y="species", all.x= TRUE)
trait_trawl_m <- select(trait_trawl, c('LCT','set_number', 'max_length_cm', 'environment', 'depth_range_max' ))

#add method to trawl 
trait_trawl_n <- merge(trait_trawl_m, method_key, by= "LCT")
trait_trawl_n <- na.omit(trait_trawl_n) #remove NA (need to deal with this later) 

#add traits to eDNA 
trait_eDNA <- merge(eDNA_data, trait_data, by.x= "LCT", by.y ="species", all.x=TRUE)
trait_eDNA_m <- select(trait_eDNA, c('LCT','set_number', 'max_length_cm', 'environment', 'depth_range_max' ))
colnames(trait_eDNA_m)[1] <- "species"

trait_eDNA_n <- merge(trait_eDNA_m, method_key, by.x='species', by.y="LCT")

colnames(trait_eDNA_n) <- c('LCT', 'set_number', 'max_length_cm','environment','depth_range_max', 'method')

traits <- rbind(trait_trawl_n,trait_eDNA_n)

traits <- na.omit(traits)
traits$max_length_cm <- as.numeric(traits$max_length_cm)
traits$max_length_cm <- as.integer(traits$max_length_cm)

#Exploratory analysis 
#Now let's plot! 


#check out ind. length info 
ggplot(trawl_catch, aes(x=length_cm))+
  geom_histogram(color="lightblue", fill="lightblue", binwidth = 4,)+
  theme_classic()

#check out environment 
ggplot(data = traits, aes(x = environment, fill = method)) +
  geom_bar() + 
  theme_classic()

#let's just look at species 
speciestraits= subset(traits, select = -c(set_number) )
speciestraits <- distinct(speciestraits)

#species length distributions 
ggplot(speciestraits, aes(max_length_cm, fill = method)) +
  stat_density(aes(y = ..density..), position = "identity", color = "black", alpha = 0.5) +
  theme_classic()

#get rid of 'both', did this by hand - time crunch 

write_csv(speciestraits,
          here("Processed_data",
               "traits",
               "speciestraits.csv"))


speciestraits2 <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "traits",
                                  "speciestraits2.csv"),
                       head=TRUE)



unique(speciestraits2$method)

ggplot(speciestraits2, aes(max_length_cm, fill = method)) +
  stat_density(aes(y = ..density..), position = "identity", color = "black", alpha = 0.5) +
  theme_classic()

#let's look at environment
ggplot(data = speciestraits2, aes(x = environment, fill = method)) +
  geom_bar() + 
  theme_classic()


ggplot(speciestraits2, aes(depth_range_max, fill = method)) +
  stat_density(aes(y = ..density..), position = "identity", color = "black", alpha = 0.5) +
  theme_classic()

