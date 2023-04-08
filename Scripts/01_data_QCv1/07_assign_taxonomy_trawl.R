#Assign Taxonomy Trawl 
#Goal:clean trawl taxonomy by using curated key 

#install.packages("taxadb")

library(plyr)
library(tidyverse)
library(tidyr)
library(taxadb)
library(readr)
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

#each species unique to trawl has an associated 'cleaned' taxonomic name 

trawl_key <- read.csv(here::here("Processed_data", #this was curated by hand
                                 "trawl",
                                 "catch_data",
                                 "taxonomy",
                                 "trawl_taxonomy_clean.csv"),
                      head=TRUE)

#Clean df to merge ####
#get rid of whitespace in trawl taxonomy key 
trawl_key$common_name <- trimws(trawl_key$common_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") 

#rename species to common_name for consistency 
trawl_spp <- trawl_spp %>% rename(common_name = species)
trawl_catch <- trawl_catch %>% rename(common_name = species)


#remove trailing spaces for both these datasets 
trawl_spp$common_name <- trimws(trawl_spp$common_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
trawl_catch$common_name <- trimws(trawl_catch$common_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

#make key dataset w. common names and tax. clean names 
#each species unique to trawl has an associated 'cleaned' taxonomic name 

#check out these weird places where 
trawl_df <- merge(trawl_spp, trawl_key, by=c("common_name"), all.x= TRUE)

#why do some have weird levels 
weird <- select(trawl_df, c('common_name', 'level'))
#manual go in and enter levels for: (even though they appear EXACTLY the same in my key)
  #Hake (Whiting)
  #Myctophid (Diaphus theta)
  #Osmeridae (Whitebait smelt)
  #seems like it has something to do with the () 
  #replace level for all non-fish as not_fish instead of NA 

#cleaned file and upload NEW 
#write_csv (trawl_df, 
#           here("Processed_data",
#                "trawl",
#                "catch_data",
#                'taxonomy',
#                "fix_taxonomy.csv"))


trawl_df <- read.csv(here::here("Processed_data",
                                   "trawl",
                                   "catch_data",
                                   "taxonomy",
                                   "fix_taxonomy1.csv"),
                        head=TRUE)


#take away all species that are not fish 

trawl_df <- subset(trawl_df, level!="not_fish")

#fix names of species ####
unique(trawl_df$species) #see species list 

fixnames <- data.frame(lapply(trawl_df, function(x) {
  gsub("Squalus acanthias", "Squalus suckleyi", x)
})) 

fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
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


#assign taxonomy to catch data set ####



trawl <- merge(trawl_catch, trawl_key, by="common_name", all.x= TRUE)

#why do some have weird levels 
weird <- select(trawl, c('common_name', 'level'))
#the issue from above does not seem to be a problem?? 

#cleaned file and upload NEW 
#write_csv (trawl, 
#           here("Processed_data",
#               "trawl",
#               "catch_data",
#              'taxonomy',
#              "fix_taxonomy2.csv"))


trawl <- read.csv(here::here("Processed_data",
                                "trawl",
                                "catch_data",
                                "taxonomy",
                                "fix_taxonomy3.csv"),
                     head=TRUE)


#take away all species that are not fish 
trawl <- subset(trawl, level!="not_fish")

#fix names of species ####
unique(trawl$species) #see species list 

fixnames <- data.frame(lapply(trawl, function(x) {
  gsub("Squalus acanthias", "Squalus suckleyi", x)
}))


fixnames <- data.frame(lapply(fixnames, function(x) {
  gsub("Scophthalmus maximus", "Reinhardtius hippoglossoides", x) #out of range, more likely Greenland halibut 
  
}))

unique(fixnames$species)

#fix common names ####

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
               "trawl_catch_clean.csv"))

#"Gadus chalcogrammus" #has some weird things going on, we are going to fix it manually
#change all "Gadus chalcogrammus to Gadus/chalcogrammus
#saved as trawl_all_cleanFINAL.csv


