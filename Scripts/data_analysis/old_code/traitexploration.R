#Trait Investigation 

library(dplyr)
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(ggplot2)

trawl_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  "catch_data",
                                  "clean_data",
                                  "fulldatasettrawlmin.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", 
                                 "eDNA",
                                 "eDNAfulldatasetmin.csv"),
                      head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                 "traits",
                                 "traitdatabase2.csv"),
                      head=TRUE)

#make new dataset with species traits attached 

trait_trawl <- merge(trawl_data, trait_data, by="species", all.x= TRUE)
trait_trawl_m <- select(trait_trawl, c('species','set_number', 'max_length_cm', 'environment' ))
#add column 
trait_trawl_m$method <- "trawl"


trait_eDNA <- merge(eDNA_data, trait_data, by.x= "LCT", by.y="species", all.x=TRUE)
trait_eDNA_m <- select(trait_eDNA, c('LCT','set_number', 'max_length_cm', 'environment' ))
trait_eDNA_m$method <- "eDNA"
colnames(trait_eDNA_m)[1] <- "species"

traits <- rbind(trait_trawl_m,trait_eDNA_m)


#select species, length, set_number, region, environment

#now make histogram of max species length 
ggplot(traits, aes(x=max_length_cm, color=method)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  theme_classic()


ggplot(trait_eDNA, aes(x=max_length_cm))+
  geom_histogram(color="darkblue", fill="lightblue")


ggplot(trait_eDNA, aes(x=common_length_cm))+
  geom_histogram(color="darkblue", fill="lightblue")



ggplot(trait_trawl, aes(x=max_length_cm))+
  geom_histogram(color="darkblue", fill="lightblue")

ggplot(trait_trawl, aes(x=common_length_cm))+
  geom_histogram(color="darkblue", fill="lightblue")

#check out environment 

ggplot(data = traits, aes(x = environment, fill = method)) +
  geom_bar() + 
  theme_classic()



newdata <- olddata %>%
          group_by(country, year) %>%
          summarise(new_variable= max(year))


