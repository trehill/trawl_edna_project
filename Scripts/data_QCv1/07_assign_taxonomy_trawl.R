#Assign Taxonomy Trawl 
#Goal:clean trawl taxonomy by using curated key 

#install.packages("taxadb")

library(plyr)
library(tidyverse)
library(tidyr)
library(taxadb)
library(dplyr)
library(here)

trawl_spp <- read.csv(here::here("Processed_data",
                                   "trawl",
                                   "catch_data",
                                   "clean_data",
                                   "trawl_catch_sum.csv"),
                        head=TRUE)

trawl_catch <- read.csv(here::here("Processed_data",
                                 "trawl",
                                 "catch_data",
                                 "clean_data",
                                 "trawl_catch.csv"),
                      head=TRUE)

#make key dataset w. common names and tax. clean names 
trawl_key <- read.csv(here::here("Processed_data", #this was curated by hand
                                 "trawl",
                                 "catch_data",
                                 "clean_data",
                                 "trawl_taxonomy_clean.csv"),
                      head=TRUE)

#first we will fix the summary df 

trawl_df <- merge(trawl_spp, trawl_key, by.x="species", by.y="common_name", all.x= TRUE)

#take away all species that are not fish 
trawl_df <- subset(trawl_df, level!="not_fish")

#clean up data 
names(trawl_df)[1] <- "common_name" #rename species columns 
names(trawl_df)[names(trawl_df) == "species.y"] <- "species"
final_trawl <- subset(trawl_df, select = -c(X))

#fix names of species 
unique(final_trawl$species) #see species list 

fixnames <- data.frame(lapply(final_trawl, function(x) {
  gsub("Squalus acanthias", "Squalus suckleyi", x)
})) 



fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))


unique(fixnames$species)

#fix common names 
unique(final_trawl$common_name) #see species list 

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Turbot", "Greenland Halibut", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Hake", "Pacific Hake", x) 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Juvenile Pollock", "Walleye Pollock", x) 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Myctophiid spp", "Myctophiid sp", x) 
  
}))


fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Unidentified Rockfish", "Sebastes sp", x) 
  
}))


fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Zoarcidae A", "Zoarcidae sp", x) 
  
}))

unique(fixnames$common_name)

#change NA to LCT 
#add column for LCT and make NA -> common_name 

fixnames$LCT = fixnames$species 

final <- fixnames %>% 
  mutate(LCT = coalesce(LCT,common_name))

write_csv(final,
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_sum_clean.csv"))


#"Gadus chalcogrammus" #has some weird things going on, we are going to fix it manually
#change all "Gadus chalcogrammus to Gadus/chalcogrammus
#saved as trawl_sum_cleanFINAL.csv


#### catch data set 

#next, we will fix the catch dataset ####

trawl <- merge(trawl_catch, trawl_key, by.x="species", by.y="common_name", all.x= TRUE)

#take away all species that are not fish 
trawl <- subset(trawl, level!="not_fish")

#clean up data 
names(trawl)[1] <- "common_name" #rename species columns 
names(trawl)[names(trawl) == "species.y"] <- "species"
final_trawl_all <- subset(trawl, select = -c(X))

#fix names of species 
unique(final_trawl_all$species) #see species list 

fixnames <- data.frame(lapply(final_trawl_all, function(x) {
  gsub("Squalus acanthias", "Squalus suckleyi", x)
}))


fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

unique(fixnames$species)

#fix common names 

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Turbot", "Greenland Halibut", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Myctophiid spp", "Myctophiid sp", x) 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Unidentified Rockfish", "Sebastes sp", x) 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Zoarcidae A", "Zoarcidae sp", x) 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Hake", "Pacific Hake", x) 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Juvenile Pollock", "Walleye Pollock", x) 
  
}))

unique(fixnames$common_name)

#change NA to LCT 
#add column for LCT 

fixnames$LCT = fixnames$species 

final <- fixnames %>% 
  mutate(LCT = coalesce(LCT,common_name))



write_csv (final, 
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_all_clean.csv"))

#"Gadus chalcogrammus" #has some weird things going on, we are going to fix it manually
#change all "Gadus chalcogrammus to Gadus/chalcogrammus
#saved as trawl_all_cleanFINAL.csv


