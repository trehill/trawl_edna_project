#Assign Taxonomy Trawl 
#Goal:clean trawl taxonomy by using curated key 

#install.packages("taxadb")

#SET-UP ####
#packages + data 
library(plyr)
library(tidyverse)
library(tidyr)
library(taxadb)
library(readr)
library(dplyr)
library(here)

#we are going to assign taxonomy to two datasets
trawl_spp <- read.csv(here::here("Processed_data", #summary trawl dataset 
                                 "trawl",
                                 "catch_data",
                                 "clean_data",
                                 "trawl_catch_sum.csv"),
                      head=TRUE)


trawl_catch <- read.csv(here::here("Processed_data", #each indv. in trawl dataset
                                   "trawl",
                                   "catch_data",
                                   "clean_data",
                                   "trawl_catch.csv"),
                        head=TRUE)

#each species unique to trawl has an associated 'cleaned' taxonomic name 

trawl_key <- read.csv(here::here("Processed_data", #this was curated by hand
                                 "trawl",
                                 "catch_data",
                                 "taxonomy",
                                 "trawl_taxonomy_clean.csv"),
                      head=TRUE)

#this dataset includes all the variations of species names in raw data of trawl catch to appropriate
#taxonomic names to use between eDNA and trawl 
#make key dataset w. common names and tax. clean names 
#each species unique to trawl has an associated 'cleaned' taxonomic name 

#Clean df to merge ####
#get rid of whitespace in trawl taxonomy key 
trawl_key$common_name <- trimws(trawl_key$common_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") 

#rename species to common_name for consistency 
trawl_spp <- trawl_spp %>% rename(common_name = species)
trawl_catch <- trawl_catch %>% rename(common_name = species)

#remove trailing spaces for both these datasets 
trawl_spp$common_name <- trimws(trawl_spp$common_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
trawl_catch$common_name <- trimws(trawl_catch$common_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

#merge key with data
trawl_df <- merge(trawl_spp, trawl_key, by=c("common_name"), all.x= TRUE)

#take away all species that are not fish 
trawl_df <- subset(trawl_df, level!="not_fish")

#fix names of species ####
unique(trawl_df$species) #see species list 
#NAs are okay here as some individuals were not identified to the species level (instead genus/family)

fixnames <- data.frame(lapply(trawl_df, function(x) {
  gsub("Squalus acanthias", "Squalus suckleyi", x) #out of date taxonomy
})) 

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Gadus chalcogrammus", "Gadus/chalcogrammus", x) #for some reason this species name does not match across datasets
  
}))

unique(fixnames$species)

#fix common names of species ####
unique(trawl_df$common_name) #see species list 

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

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Zoarcid \"A\"", "Zoarcidae sp", x) 
  
}))

unique(fixnames$common_name)

#change NA to LCT 
#add column for LCT and make NA -> species name 
#the LCT groupings for the trawl are just "Genus sp"

fixnames$LCT = fixnames$species 

final <- fixnames %>% 
  mutate(LCT = coalesce(LCT,common_name))

#remove Sebastes sp + Corphaenidoes sp
final <- subset(final, LCT != 'Sebastes sp') #we decided that these spp. appear only once and don't have any measurements (length/weight)
#it is thus excluded from the analysis (similar to eDNA singletons)
final <- subset(final, LCT != 'Corphaenidoes sp')

write_csv(final,
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_sum_clean.csv"))

####Manual editing MAY be required ###################

#Even though there is code above to change this, Gadus chalcogrammus seems to not work well...
#Instead, view this file and ensure that the / is added
#If not, change this manually!! (it should work though, I've just had problems with it in the past)
#"Gadus chalcogrammus" #has some weird things going on, we are going to fix it manually
#change all "Gadus chalcogrammus to Gadus/chalcogrammus

#Fix speacies for catch dataset ####
unique(trawl_df$species) #see species list 
#NAs are okay here as some individuals were not identified to the species level (instead genus/family)

fixnames <- data.frame(lapply(trawl_df, function(x) {
  gsub("Squalus acanthias", "Squalus suckleyi", x) #out of date taxonomy
})) 

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Gadus chalcogrammus", "Gadus/chalcogrammus", x) #for some reason this species name does not match across datasets
  
}))

unique(fixnames$species)

#fix common names of species ####
unique(trawl_df$common_name) #see species list 

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

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Zoarcid \"A\"", "Zoarcidae sp", x) 
  
}))


unique(fixnames$common_name)

#change NA to LCT 
#add column for LCT and make NA -> species name 
#the LCT groupings for the trawl are just "Genus sp"

fixnames$LCT = fixnames$species 

final <- fixnames %>% 
  mutate(LCT = coalesce(LCT,common_name))

#remove Sebastes sp + Corphaenidoes sp
final <- subset(final, LCT != 'Sebastes sp') #we decided that these spp. appear only once and don't have any measurements (length/weight)
#it is thus excluded from the analysis (similar to eDNA singletons)
final <- subset(final, LCT != 'Corphaenidoes sp')

write_csv(final,
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_sum_clean.csv"))

####Manual editing MAY be required ###################

#Even though there is code above to change this, Gadus chalcogrammus seems to not work well...
#Instead, view this file and ensure that the / is added
#If not, change this manually!! (it should work though, I've just had problems with it in the past)
#"Gadus chalcogrammus" #has some weird things going on, we are going to fix it manually
#change all "Gadus chalcogrammus to Gadus/chalcogrammus

#Fix species names in catch dataset ####
#merge key with data
trawl_df <- merge(trawl_catch, trawl_key, by=c("common_name"), all.x= TRUE)

#take away all species that are not fish 
trawl_df <- subset(trawl_df, level!="not_fish")

#fix names of species ####
unique(trawl_df$species) #see species list 
#NAs are okay here as some individuals were not identified to the species level (instead genus/family)

fixnames <- data.frame(lapply(trawl_df, function(x) {
  gsub("Squalus acanthias", "Squalus suckleyi", x) #out of date taxonomy
})) 

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Gadus chalcogrammus", "Gadus/chalcogrammus", x) #for some reason this species name does not match across datasets
  
}))

unique(fixnames$species)

#fix common names of species ####
unique(trawl_df$common_name) #see species list 

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
#add column for LCT and make NA -> species name 
#the LCT groupings for the trawl are just "Genus sp"

fixnames$LCT = fixnames$species 

final <- fixnames %>% 
  mutate(LCT = coalesce(LCT,common_name))

#remove Sebastes sp + Corphaenidoes sp
final <- subset(final, LCT != 'Sebastes sp') #we decided that these spp. appear only once and don't have any measurements (length/weight)
#it is thus excluded from the analysis (similar to eDNA singletons)
final <- subset(final, LCT != 'Corphaenidoes sp')

write_csv(final,
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_catch_clean.csv"))

####Manual editing MAY be required ###################

#Even though there is code above to change this, Gadus chalcogrammus seems to not work well...
#Instead, view this file and ensure that the / is added
#If not, change this manually!! (it should work though, I've just had problems with it in the past)
#"Gadus chalcogrammus" #has some weird things going on, we are going to fix it manually
#change all "Gadus chalcogrammus to Gadus/chalcogrammus