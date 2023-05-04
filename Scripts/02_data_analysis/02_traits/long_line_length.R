#Long-line surveys of length 
#SET-Up ####
#Load libraries 

library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)

#Read in files 
#Long-line surveys: https://open.canada.ca/data/en/dataset/945e0f13-119b-451b-9038-50c6eb641aef

#data sourced form https://open.canada.ca/data/en/dataset/8345a8f5-0ba1-4eb5-b2c0-8410079154bb
#inside north hard bottom longline surveys
synop1 <- read.csv(here::here("Raw_data", 
                              "long_line_surveys",
                              "INS-N_biology.csv"),
                   head=TRUE)

#data sourced from https://open.canada.ca/data/en/dataset/ad921d10-363f-45fb-b0ce-05304fb91386
#inside south hard bottom longline surveys 
synop2 <- read.csv(here::here("Raw_data", 
                              "long_line_surveys",
                              "INS-S_biology.csv"),
                   head=TRUE)
#data sourced from https://open.canada.ca/data/en/dataset/524fde54-1d93-4d22-bb83-df542780a719
#outside north hard bottom longline surveys 
synop3 <- read.csv(here::here("Raw_data", 
                              "long_line_surveys",
                              "OUT-N_biology.csv"),
                   head=TRUE)

#data sourced from https://open.canada.ca/data/en/dataset/3cdc1ad5-70e5-4fac-865d-e583e54d15df
#outside south hard bottom longline surveys 
synop4 <- read.csv(here::here("Raw_data", 
                              "long_line_surveys",
                              "OUT-N_biology.csv"),
                   head=TRUE)


#merge all synoptic trawl survey datasets together 
synop <- rbind(synop1, synop2, synop3, synop4) #combine both longline datasets (have the same row names)

#detection data for all species detected (in eDNA and trawl)
detection <- read.csv(here::here("Processed_data", 
                                 "datasets",
                                 "detections_all.csv"),
                      head=TRUE)

trait_db <- read.csv(here::here("Processed_data", 
                                "traits",
                                "traitdatabase.csv"),
                     head=TRUE)

#Format synoptic data  ####
#so we can merge datasets easily 

#create copy of data we can manipulate 
syn <- synop

#convert total length from mm to cm 
#we are interested in 'total length' data 
#this column is labelled 'Total.length..mm.'
#we need to change this column to cm (this is what unit our individual trawl data uses)

#(1) convert total length from mm to cm 

#rename column to something easier to work with 

syn <- syn %>% rename(
  total_length_mm = c('Total.length..mm.')) #rename length column to 'total_length_mm

syn <- syn %>% rename(
  fork_length_mm = c('Fork.length..mm.')) #rename length column to 'total_length_mm

syn$fork_length_cm <- syn$fork_length_mm/10 #convert mm to cm by creating a new column 
syn$total_length_cm <- syn$total_length_mm/10 #convert mm to cm by creating a new column 

#we are also interested in the species names that these lengths correspond to 
#this column is 'Scientific.name', corresponds to LCT of our detection dataset 
#will need to figure out how to get mean length data for LCT that are not down to the spp. level 
#this column is all in caps so we need to change this to only the first letter is capitalized

#(2) convert caps to lower case for scientific names 

syn$Scientific.name <- tolower(syn$Scientific.name) #converts whole name to lowercase 
syn$Scientific.name <- str_to_sentence(syn$Scientific.name) #converts only first letter of name to uppercase

#(3) format some names that should match 
#Gadus chalcogrammus to match detection name (insert /) 
#Gadus/chalcogrammus <- detection dataset notation 
syn <- data.frame(lapply(syn, function(x) {
  gsub("Gadus chalcogrammus","Gadus/chalcogrammus", x) }))

#Zoarcidae = Zoercidae spp
syn <- data.frame(lapply(syn, function(x) {
  gsub("Zoarcidae","Zoarcidae sp", x) }))

#combine info for LCTs
syn <- data.frame(lapply(syn, function(x) {
  gsub("Sebastes caurinus","Sebastes caurinus/maliger", x) }))

syn <- data.frame(lapply(syn, function(x) {
  gsub("Sebastes maliger","Sebastes caurinus/maliger", x) }))

syn <- syn <- data.frame(lapply(syn, function(x) {
  gsub("Coryphaenoides cinerus","Corphaenidoes sp", x) }))

syn <- syn <- data.frame(lapply(syn, function(x) {
  gsub("Coryphaenoides acrolepis","Corphaenidoes sp", x) }))

syn <- syn <- data.frame(lapply(syn, function(x) {
  gsub("Coryphaenoides filifer","Corphaenidoes sp", x) }))

syn <- data.frame(lapply(syn, function(x) {
  gsub("Lumpenus sagitta","Xiphister atropurpureus/mucosus", x) }))

syn <- data.frame(lapply(syn, function(x) {
  gsub("Poroclinus rothrocki","Xiphister atropurpureus/mucosus", x) }))


#(4) figure out which species are missing from synoptic trawl that are present in our dataset 
#Find overlapping LCT found using both methods 
#syn_spp <- syn$Scientific.name #all species in synoptic trawl dataset 
#det_spp <- detection$LCT #all species in our detection dataset

#missing <- detection[  !(detection$LCT %in% syn_spp), ] 
#unique(missing$LCT) #about half of our spp are missing from this dataset 

#searched missing speciesh in ITIS to see if they might have a real match with the synoptic data
#smelts = sprinchus starksi, let's change this across the datasets so they match 

syn <- data.frame(lapply(syn, function(x) {
  gsub("Osmeridae","Spirinchus starksi", x) }))

#flounders - Reinhardtius evermann, corresponds to ATHERESTHES STOMIAS + "PLATICHTHYS STELLATUS"   

syn <- data.frame(lapply(syn, function(x) {
  gsub("Atheresthes stomias","Reinhardtius evermanni", x) }))

syn <- data.frame(lapply(syn, function(x) {
  gsub("Platichthys stellatus","Reinhardtius evermanni", x) }))

#halibut/turbot = Reinhardtius hippoglossoides, corresponds to  "HIPPOGLOSSUS STENOLEPIS" 

syn <- data.frame(lapply(syn, function(x) {
  gsub("Hippoglossus stenolepis","Reinhardtius hippoglossoides", x) }))

#try again...

syn_spp <- syn$Scientific.name #all species in synoptic trawl dataset 
det_spp <- detection$LCT #all species in our detection dataset

missing <- detection[  !(detection$LCT %in% syn_spp), ] 
unique(missing$LCT)
