#Synoptic trawl data 
#goal: determine average/mean length of species caught in trawl + eDNA to use 
#in data analysis of length density plots, average/mean length will be sourced from 
#(a) synoptic trawl data, (b) common length, last resort- (c) maximum spp. length from FishBase

#SET-Up ####
#Load libraries 
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)

#Read in files 
#Synoptic trawls 
#data sourced form https://open.canada.ca/data/en/dataset/557e42ae-06fe-426d-8242-c3107670b1de
#bottom trawl survey, west coast of vancouver island 
synop1 <- read.csv(here::here("Raw_data",  
                              "synoptic_trawl",
                              "WCVI_biology.csv"),
                   head=TRUE)

#data sourced from https://open.canada.ca/data/en/dataset/d880ba18-8790-41a2-bf73-e9247380759b
#bottom trawl survey in the straight of georgia
synop2 <- read.csv(here::here("Raw_data",  
                              "synoptic_trawl",
                              "SOG_biology.csv"),
                   head=TRUE)
#data sourced from https://open.canada.ca/data/en/dataset/86af7918-c2ab-4f1a-ba83-94c9cebb0e6c
#bottom trawl survey in queen charlotte sound
synop3 <- read.csv(here::here("Raw_data",  
                              "synoptic_trawl",
                              "QCS_biology.csv"),
                   head=TRUE)

#data sourced from https://open.canada.ca/data/en/dataset/5ee30758-b1d6-49fe-8c4e-5136f4b39ad1
#bottom trawl survey in west coast Haida Gwaii
synop4 <- read.csv(here::here("Raw_data",  
                              "synoptic_trawl",
                              "WCHG_biology.csv"),
                   head=TRUE)

#data sourced from https://open.canada.ca/data/en/dataset/780a1c02-1f9c-4994-bc70-a0e9ef8e3968
#bottom trawl survey in Hecate straight
synop5 <- read.csv(here::here("Raw_data",  
                              "synoptic_trawl",
                              "HS_biology.csv"),
                   head=TRUE)


#merge all synoptic trawl survey datasets together 
synop <- rbind(synop1, synop2, synop3, synop4, synop5) #combine both synoptic datasets (have the same row names)

#detection data for all species detected (in eDNA and trawl)
detection <- read.csv(here::here("Processed_data", 
                                 "datasets",
                                 "detections_all.csv"),
                      head=TRUE)

trait_db <- read.csv(here::here("Processed_data", #has maximum spp. length data
                                "datasets",
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
  total_length_mm = c('Total.length..mm.')) %>% #rename length column to 'total_length_mm
  rename(fork_length_mm = c('Fork.length..mm.')) %>% #rename fork length column 
  rename(standard_length_mm = c('Standard.length..mm.')) #rename standard length column 


syn$fork_length_cm <- syn$fork_length_mm/10 #convert mm to cm by creating a new column 
syn$total_length_cm <- syn$total_length_mm/10 #convert mm to cm by creating a new column 
syn$standard_length_mm <- syn$standard_length_mm/10 #convert mm to cm by creating a new column 

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
#LCTs are composed of multiple spp, so we change sp name to LCT name for grouped sp
syn <- data.frame(lapply(syn, function(x) {
  gsub("Sebastes caurinus","Sebastes caurinus/maliger", x) }))

syn <- data.frame(lapply(syn, function(x) {
  gsub("Sebastes maliger","Sebastes caurinus/maliger", x) }))

#pricklebacks, Lempenus sagitta and Poroclinus rothrocki are both pricklebacks same as Xiphister
syn <- data.frame(lapply(syn, function(x) {
  gsub("Lumpenus sagitta","Xiphister atropurpureus/mucosus", x) }))

syn <- data.frame(lapply(syn, function(x) {
  gsub("Poroclinus rothrocki","Xiphister atropurpureus/mucosus", x) }))

#(4) figure out which species are missing from synoptic trawl that are present in our dataset 
#Find overlapping LCT found using both methods 
syn_spp <- syn$Scientific.name #all species in synoptic trawl dataset 
det_spp <- detection$LCT #all species in our detection dataset

missing <- detection[  !(detection$LCT %in% syn_spp), ] 
unique(missing$LCT) #about half of our spp are missing from this dataset 

#searched missing speciesh in ITIS to see if they might have a real match with the synoptic data
#change names for closest matches 

#smelts = sprinchus starksi, let's change this across the datasets so they match 

syn <- data.frame(lapply(syn, function(x) {
  gsub("Osmeridae","Spirinchus starksi", x) }))

#flounders - Reinhardtius evermanni, corresponds to ATHERESTHES STOMIAS + "PLATICHTHYS STELLATUS"   

syn <- data.frame(lapply(syn, function(x) {
  gsub("Atheresthes stomias","Reinhardtius evermanni", x) }))

syn <- data.frame(lapply(syn, function(x) {
  gsub("Platichthys stellatus","Reinhardtius evermanni", x) }))

#halibut/turbot = Reinhardtius hippoglossoides, corresponds to  "HIPPOGLOSSUS STENOLEPIS" 

syn <- data.frame(lapply(syn, function(x) {
  gsub("Hippoglossus stenolepis","Reinhardtius hippoglossoides", x) }))

#pacific sandlance Ammodyte hexapterus corresponds to Ammodytes personatus

syn <- data.frame(lapply(syn, function(x) {
  gsub("Ammodytes personatus","Ammodytes hexapterus", x) }))

#try again...to see what other spp are missing from the synoptic set to our dataset 
syn_spp <- syn$Scientific.name #all species in synoptic trawl dataset 
det_spp <- detection$LCT #all species in our detection dataset

missing <- detection[  !(detection$LCT %in% syn_spp), ] 
unique(missing$LCT) #these species are missing from synoptic trawl dataset
#they do not appear in the synoptic data, we will use common lengths or worst case maximum spp
#lengths from FishBase 


#missing species: 
#Diaphus theta: small fish, pelagic, headlight fish 
#Engraulis mordax: anchovy, pelagic
#Entosphenus tridentatus: lamprey, common length= 41.0cm 
#Oncorhynchus kisutch: coho salmon, pelagic 
#pholis laeta: gunnel, demersal
#Phytichthys chirus: demersal eelpout 
#Sebastes mystinus: reef-associated
#Leptoclinus maculatus: eelpouts
#Mallotus villosus: 

#these same species are not found in the long-line surveys either (see long_line_length.R)

data <- syn #this dataset is now 'harmonized' to our detection + trait database (- problem children)

#Convert Fork length to Total length ####

fork <- select(data, c('Scientific.name', 'fork_length_cm', 'Specimen.identifer'))
fork <- fork %>% drop_na(fork_length_cm) #only include species with available fork length data
fork$fork_length_cm <- as.numeric(fork$fork_length_cm)
unique(fork$Scientific.name)

#Fixing species that have fork length data instead of total length data 

#Species 1: Oncorhynchus tshawytscha 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=244&GenusName=Oncorhynchus&SpeciesName=tshawytscha&fc=76
#TL= 1.034FL
fork_ot <- subset(fork, Scientific.name == 'Oncorhynchus tshawytscha')
fork_ot$fork_to_total_length_cm <- 1.034*fork_ot$fork_length_cm


#Species 2: Clupea pallasii 
#no conversion from FL to TL on fishbase
#convert based on: https://onlinelibrary.wiley.com/doi/full/10.1111/jfb.14105
#LT =1.110LF – 0.323

fork_cp <- subset(fork, Scientific.name == 'Clupea pallasii')
fork_cp$fork_to_total_length_cm <- 1.110*fork_cp$fork_length_cm-0.323 

#Species 3:Engraulis mordax 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=1664&GenusName=Engraulis&SpeciesName=mordax&fc=454
#LT =1.083LF 

fork_em <- subset(fork, Scientific.name == 'Engraulis mordax')
fork_em$fork_to_total_length_cm <- 1.083*fork_em$fork_length_cm 

#Species 4: Oncorhynchus gorbuscha 
#convert based on: https://www.fishbase.se/popdyn/LLRelationshipList.php?ID=240&GenusName=Oncorhynchus&SpeciesName=gorbuscha&fc=76
#LT =1.028FL

fork_og <- subset(fork, Scientific.name == 'Oncorhynchus gorbuscha')
fork_og$fork_to_total_length_cm <- 1.028*fork_og$fork_length_cm

#Species 5:Thaleichthys pacificus 
#not available in fishbase
#convert based on: https://onlinelibrary.wiley.com/doi/full/10.1111/j.0022-1112.2005.00741.x
#this is the conversion for thawed specimens after freezing 
#LT =1.078FL+0.451

fork_tp <- subset(fork, Scientific.name == 'Thaleichthys pacificus')
fork_tp$fork_to_total_length_cm<- 1.078*fork_tp$fork_length_cm + 0.451

#Species 6: Mallotus villosus 
#no FL to TL available on fishbase 
#convert based on: https://onlinelibrary.wiley.com/doi/full/10.1111/j.0022-1112.2005.00741.x
#this is the conversion for thawed specimens after freezing 
#LT =1.075FL+0.449

fork_mv <- subset(fork, Scientific.name == 'Mallotus villosus')
fork_mv$fork_to_total_length_cm <- 1.075*fork_mv$fork_length_cm + 0.449

#Species 7: Oncorhynchus kisutch 
#no FL to TL available on fishbase 
#convert based on: https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/1548-8640%281995%29057%3C0250%3ATLTFLR%3E2.3.CO%3B2
#TL = 1.0822(FL) – 0.0861

fork_ok <- subset(fork, Scientific.name == 'Oncorhynchus kisutch')
fork_ok$fork_to_total_length_cm <- 1.0822*fork_ok$fork_length_cm + 0.0861


#Species 8: Anoplopoma fimbria 
#on fishbase
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=512&GenusName=Anoplopoma&SpeciesName=fimbria&fc=270
#TL = 1.025(FL) 

fork_af <- subset(fork, Scientific.name == 'Anoplopoma fimbria')
fork_af$fork_to_total_length_cm <- 1.025*fork_af$fork_length_cm 


#Species 9: Oncorhynchus nerka 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=243&GenusName=Oncorhynchus&SpeciesName=nerka&fc=76
##TL = FL/0.923 
fork_on <- subset(fork, Scientific.name == 'Oncorhynchus nerka')
fork_on$fork_to_total_length_cm <- (fork_on$fork_length_cm / 0.923)


#Species 10: Allosmerus elongatus 
#no length-length table on fishbase
#convert based on: https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/348681.pdf
##TL = FL/0.909
fork_ae <- subset(fork, Scientific.name == 'Allosmerus elongatus')
fork_ae$fork_to_total_length_cm <- (fork_ae$fork_length_cm / 0.909)

#Species 11: 	Oncorhynchus keta 
#FL to TL available on fishbase 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=241&GenusName=Oncorhynchus&SpeciesName=keta&fc=76
##TL = 1.017FL
fork_ok2 <- subset(fork, Scientific.name == 'Oncorhynchus keta')
fork_ok2$fork_to_total_length_cm <- (fork_ok2$fork_length_cm*1.017)

#Species 12: 	Alosa sapidissima 
#FL to TL available on fishbase 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=1584&GenusName=Alosa&speciesName=sapidissima&fc=43
##TL = 1.127FL
fork_as <- subset(fork, Scientific.name == 'Alosa sapidissima')
fork_as$fork_to_total_length_cm <- (fork_as$fork_length_cm*1.127)

#Species 12: Gadus/chalcogrammus
#FL to TL available on fishbase 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=318&GenusName=Gadus&SpeciesName=chalcogrammus&fc=183
#TL=FL/0.904
fork_gc <- subset(fork, Scientific.name == 'Gadus/chalcogrammus')
fork_gc$fork_to_total_length_cm <- (fork_gc$fork_length_cm/0.904)

#Species 13: Hexagrammos decagrammus, https://fishbase.se/popdyn/LLRelationshipList.php?ID=4032&GenusName=Hexagrammos&SpeciesName=decagrammus&fc=271
#FL to TL available on Fishbase 
#TL=FL/0.985
fork_hd <- subset(fork, Scientific.name == 'Hexagrammos decagrammus')
fork_hd$fork_to_total_length_cm <- (fork_hd$fork_length_cm/0.985)

#Species 14: Merluccius productus
#https://fishbase.se/popdyn/LLRelationshipList.php?ID=326&GenusName=Merluccius&SpeciesName=productus&fc=184
#TL = FL*1.010

fork_mp <- subset(fork, Scientific.name == 'Merluccius productus')
fork_mp$fork_to_total_length_cm <- (fork_mp$fork_length_cm*1.010)

#Species 15: Oncorhynchus tshawytscha
#https://fishbase.se/popdyn/LLRelationshipList.php?ID=244&GenusName=Oncorhynchus&SpeciesName=tshawytscha&fc=76
#TL = FL/0.967

fork_15 <- subset(fork, Scientific.name == 'Oncorhynchus tshawytscha')
fork_15$fork_to_total_length_cm <- (fork_15$fork_length_cm/0.967)

#Species 16: Sebastes caurinus
#https://fishbase.se/popdyn/LLRelationshipList.php?ID=3957&GenusName=Sebastes&SpeciesName=caurinus&fc=573
#TL = FL
fork_16 <- subset(fork, Scientific.name == 'Sebastes caurinus')
fork_16$fork_to_total_length_cm <- fork_16$fork_length_cm 

#Species 17: Sebastes crameri
#https://fishbase.se/popdyn/LLRelationshipList.php?ID=3962&GenusName=Sebastes&SpeciesName=crameri&fc=573
#TL = 1.023*FL
fork_17 <- subset(fork, Scientific.name == 'Sebastes crameri')
fork_17$fork_to_total_length_cm <- (fork_17$fork_length_cm*1.023)

#Species 18: Sebastes flavidus
#https://fishbase.se/popdyn/LLRelationshipList.php?ID=503&GenusName=Sebastes&SpeciesName=flavidus&fc=573
#TL = FL/0.983
fork_18 <- subset(fork, Scientific.name == 'Sebastes flavidus')
fork_18$fork_to_total_length_cm <- (fork_18$fork_length_cm/0.983)

#Species 19: Ammodytes hexapterus
#https://fishbase.se/popdyn/LLRelationshipList.php?ID=3822&GenusName=Ammodytes&SpeciesName=hexapterus&fc=402
#TL=FL*1.012

fork_19 <- subset(fork, Scientific.name == 'Sebastes flavidus')
fork_19$fork_to_total_length_cm <- (fork_19$fork_length_cm*1.012)

#Combine
length<- rbind(fork_ae, fork_af, fork_as, fork_cp, fork_em, fork_ot, fork_mv, fork_og, fork_ok, fork_on, fork_tp, fork_ok, fork_gc, fork_hd, fork_mp
               , fork_15, fork_16, fork_17, fork_18, fork_19)

#Combine with syn data 
length <- select(length, c('Specimen.identifer','Scientific.name', 'fork_to_total_length_cm'))
combined <- merge(data, length, by=c('Specimen.identifer', 'Scientific.name'), all.x=TRUE)

#If total_length_cm is NA, replace with fork_to_total_length value 
combined$total_length_cm <- as.numeric(combined$total_length_cm)
combined <- combined %>% 
  mutate(total_length_cm = coalesce(total_length_cm,fork_to_total_length_cm))

data <- combined

#Create mean length per species based on total length synoptic data ####
data <- data %>% drop_na(total_length_cm) #remove NA values in length column 

means <- data %>% #create new df of total length means per species in synoptic trawl
  group_by(Scientific.name) %>%
  dplyr::summarise(total_mean_length_cm = mean(total_length_cm))

#Merge synoptic means with trait database file ####
#change Scientific name column to LCT 
mean_data <- select(means, c('total_mean_length_cm', 'Scientific.name' ))
colnames(mean_data) <- c('total_mean_length_cm', 'LCT') #renaming column names to merge better 

total_lengths <- merge(trait_db, mean_data, by=c('LCT'), all.x=TRUE)

#Let's deal with the species that have no info in synoptic trawls (do not appear)

#THESE SPECIES: 
#Engraulis mordax: anchovy, pelagic, use common length from fishbase instead
#Entosphenus tridentatus: lamprey, use common length from fishbase instead
#Oncorhynchus kisutch: coho salmon, pelagic, use common length from fishbase instead
#Mallotus villosus: use common length from fishbase instead

#Use common species lengths to fill this column in trait database 
#first use common lengths 
final <- total_lengths %>% 
  mutate(total_mean_length_cm = coalesce(total_mean_length_cm, common_length_cm))

#still missing: 
#Diaphus theta: small fish, pelagic, headlight fish 
#Pholis laeta: gunnel, demersal
#Phytichthys chirus: demersal eelpout 
#Sebastes mystinus: reef-associated
#Leptoclinus maculatus: eelpouts

#we will fill the remaining gaps in length information using maximum species length from fishbase
final <- final  %>% 
  mutate(total_mean_length_cm = coalesce(total_mean_length_cm, max_length_cm))


#Save! ####
write_csv(final,
          here("Processed_data",
               "datasets",
               "traits",
               "traits_mean_lengths.csv")) 



