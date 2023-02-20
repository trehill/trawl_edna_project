#Trait Database Curation 
#Goal: take species list (from trawl, 12se, 12su) and search rFishBase for 
#traits (including: length (for 12s), habitat preference, shoaling behaviour etc.)
#Author: Tessa Rehill + Anya Mueller 

#Set-Up ####
#instal if necessary by removing # and running code 
#install.packages("rfishbase",repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"),type="source") 
#install.packages("tidyverse")
#install.packages("taxize")
#install.packages("here")

#solutions to rFishBase not working: 
# updated package (done)
# updated R (done)
# install.packages("devtools")
# devtools::install_github("ropensci/rfishbase")

#load packages
library(rfishbase) 
library(tidyverse)
library(taxize)
library(here)

#import species list data 
#these datasets might change later but for now we can start writing a script 
sp_12se <- read.csv(here::here("Processed_data",
                                   "eDNA",
                                   "12s",
                                    "12s_e",
                                    "LCTassignment",
                                   "taxonomy_groups_12s_eDNA.csv"),
                        head=TRUE)

sp_12su <- read.csv(here::here("Processed_data", 
                               "eDNA",
                               "12s",
                               "12s_u",
                               "LCTassignment",
                               "taxonomy_groups_12u_eDNA.csv"),
                    head=TRUE)


sp_trawl <- read.csv(here::here("Processed_data",
                               "trawl",
                               "catch_data",
                               "clean_data",
                               "trawl_taxonomy_clean.csv"),
                    head=TRUE)

#12se Trait Database ####
#Ensure species names are queriable in FishBase and SealifeBase

sp_12se <- sp_12se[sp_12se$level == 'species',] 
#we're just going to get traits for the species we know at the species level (instead of family, genus etc.)

#ensure that species names are character class 
glimpse(sp_12se) 

#clean up and select data of interest
taxa_12se <- sp_12se %>% 
  dplyr::rename_with(~(gsub("X.", "", .))) %>% #fix up column names 
  select("LCT", "class","order","family","genus","species") %>% #select columns of interest
  distinct() #pull only distinct rows

#VALIDATE NAMES
#need to make sure that the names of the fish are correct so that they will pull up information when queried against the database
#validate_names allows us to run the list against the fishbase and sealifebase database to see if our names match any entries
#a list is produced that has NAs where the fish names don't match those in the database
#this only works for specie names
#make one object for each database
sp12se_fb <- validate_names(taxa_12se$species, server = "fishbase") 
sp12se_fb
sp12se_sl <- validate_names(taxa_12se$species, server = "sealifebase")
sp12se_sl

#alternative approach (don't run if above worked?)
sp12se_valid <- validate_names(
  taxa_12se$species,
  server = getOption("FISHBASE_API", "fishbase"))

#count up NAs and see how much is accounted for
sum(is.na(sp12se_fb)) 
sum(is.na(sp12se_sl)) 

#more name validation is needed here

#SEARCH FOR TRAITS
#once names are validated, we can now search rFishBase
#collecting together all of the species level data for the fish list
#first we group together the ecosystem location data

#ecology() and species() are tables from rfishbase
taxa <- sp12se_valid #change this based on which taxa you are finding traits for 

fb.eco <-ecology(taxa, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic"), server = "fishbase")

fb.sp <-species(taxa,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "fishbase")

fb.lar <- larvae(taxa, field = c("Species","PlaceofDevelopment"))

#merge into one data.frame
fb.info <-list(fb.eco, fb.sp, fb.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "fishbase")
fb.info

#ecology() and species() are tables from sealifebase
sl.eco <-ecology(taxa, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic"), server = "sealifebase")

sl.sp <-species(taxa,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "sealifebase")

sl.lar <- larvae(taxa, field = c("Species","PlaceofDevelopment"))

#merge the ecosystem location data into one data.frame
sl.info <-list(sl.eco, sl.sp, sl.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "sealifebase")
sl.info

#add columns for the binding of the data frames
sl.info <- sl.info %>% mutate("Mesopelagic"=NA, "Bathypelagic"=NA, "Abyssopelagic"=NA, "Hadopelagic"=NA)

#combine the two server's information
info <- rbind(sl.info, fb.info)

#change the binary data into characters so that we can then collapse all theses columns into one
info.wtrpos<-info %>% select("Species","Benthic", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic") %>% mutate(Benthic = ifelse(Benthic==1,"benthic", NA),Demersal = ifelse(Demersal==1,"demersal", NA),Pelagic = ifelse(Pelagic==1,"pelagic", NA),Megabenthos = ifelse(Megabenthos==1,"megabenthos", NA),Macrobenthos = ifelse(Macrobenthos==1,"macrobenthos", NA), Meiobenthos = ifelse(Meiobenthos==1,"meiobenthos", NA),Epipelagic = ifelse(Epipelagic==1,"epipelagic", NA),Mesopelagic = ifelse(Mesopelagic==1,"mesopelagic", NA),Bathypelagic = ifelse(Bathypelagic==1,"bathypelagic", NA),Abyssopelagic = ifelse(Abyssopelagic==1,"abyssopelagic", NA),Hadopelagic = ifelse(Hadopelagic==1,"hadopelagic", NA)) %>% unite(adult_water_position, Benthic:Hadopelagic,sep=", ", na.rm = T) %>% select(Species,adult_water_position)

#change the binary data into characters so that we can then collapse all theses columns into one
info.adltmtl <- info %>% select(Species, Mobile, Sessile) %>%mutate(Mobile = ifelse(Mobile==1,"mobile", NA),Sessile = ifelse(Sessile==1,"sessile", NA)) %>% unite(adult_motility, Mobile, Sessile,sep=", ", na.rm = T) %>% select(Species,adult_motility)

#merge all the dataframes into one
info<-info %>% select("Species","server","DemersPelag", "DepthRangeShallow", "DepthRangeDeep", "PlaceofDevelopment")
info <-list(info, info.adltmtl, info.wtrpos) %>% reduce(left_join, by = "Species")

#add source column and taxonomic resolution, change coloumn name for merging purposes, remove the server column
info <-info %>% mutate(DemersPelag_source = ifelse(is.na(DemersPelag),"",server), DemersPelag_taxonomic_resolution = ifelse(is.na(DemersPelag),"","species"), DepthRangeShallow_source = ifelse(is.na(DepthRangeShallow),"",server), DepthRangeShallow_taxonomic_resolution = ifelse(is.na(DepthRangeShallow),"","species"), DepthRangeDeep_source = ifelse(is.na(DepthRangeDeep),"",server),DepthRangeDeep_taxonomic_resolution = ifelse(is.na(DepthRangeDeep),"","species"),adult_motility_source = ifelse(adult_motility=="","",server),adult_motility_taxonomic_resolution = ifelse(adult_motility=="","","species"),adult_water_position_source = ifelse(adult_water_position=="","",server),adult_water_position_taxonomic_resolution = ifelse(adult_water_position=="","","species"),PlaceofDevelopment_source = ifelse(is.na(PlaceofDevelopment),NA,server),PlaceofDevelopment_taxonomic_resolution = ifelse(is.na(PlaceofDevelopment),NA,"species")) %>% dplyr::rename("Species_fixed"="Species") %>% select(-(server))

#join the trait information to the original data
traits_12se<-left_join(sp_12se,info, by = "LCT") 

#replace the blank spaces with NAs
traits_12se <- as.data.frame(lapply(traits_12se, function(x) { ifelse(x=='', NA, x) }))

#filter so have only one species per row 
traits_12se<-traits_12se %>% select(-(row_names),-(subject)) %>% distinct()
traits_12se

#export csv
write_csv(traits_12se,
          here("Processed_data",
               "traits",
               "traits12se.csv"))

#SEARCH FOR BODY SIZE 
#pull length data from databases
fb_length<-species(taxa, field=c("Species","Length","LTypeMaxM"), server = "fishbase") 
fb_length<-fb_length %>% mutate(server= "fishbase")
sl_length<-species(taxa,field=c("Species","Length","LTypeMaxM"), server = "sealifebase")
sl_length<-sl_length %>% mutate(server = "sealifebase")

#merge
length.data<-rbind(fb_length,sl_length)
length.data

#replace the blank spaces with NAs
length.data<- as.data.frame(lapply(length.data, function(x) { ifelse(x=='', NA, x) }))
length.data

#join the trait information to the current database data 
db_12se <-read_csv(here::here("Processed_data",
                              "traits",
                              "traits12se.csv"))

db_12se_lengths<-left_join(db_12se,length.data, by = "LCT") 

#export csv
write_csv(db_12se_lengths,
          here("Processed_data",
               "traits",
               "traits12se_lengths.csv"))

#12su Trait Database ####
#Ensure species names are queriable in FishBase and SealifeBase

sp_12se <- sp_12su[sp_12su$level == 'species',] 
#we'ru just going to get traits for the species we know at the species level (instead of family, genus etc.)

#ensure that species names are character class 
glimpse(sp_12su) 

#clean up and select data of interest
taxa_12su <- sp_12su %>% 
  dplyr::rename_with(~(gsub("X.", "", .))) %>% #fix up column names 
  select("LCT", "class","order","family","genus","species") %>% #select columns of interest
  distinct() #pull only distinct rows

#VALIDATE NAMES
#need to make sure that the names of the fish are correct so that they will pull up information when queried against the database
#validate_names allows us to run the list against the fishbase and sealifebase database to see if our names match any entries
#a list is produced that has NAs where the fish names don't match those in the database
#this only works for specie names
#make one object for each database
sp12su_fb <- validate_names(taxa_12su$species, server = "fishbase") #should this be species or LCT?
sp12su_fb
sp12su_sl <- validate_names(taxa_12su$species, server = "sealifebase")
sp12su_sl

#alternative approach (don't run if above worked?)
sp12su_valid <- validate_names(
  taxa_12su$species,
  server = getOption("FISHBASE_API", "fishbase"))

#count up NAs and see how much is accounted for
sum(is.na(sp12su_fb)) 
sum(is.na(sp12su_sl)) 

#more name validation is needed here

#SEARCH FOR TRAITS
#once names are validated, we can now search rFishBase
#collecting together all of the species level data for the fish list
#first we group together the ecosystem location data

#ecology() and species() are tables from rfishbase
taxa <- taxa_12su$species #change this based on which taxa you are finding traits for 

fb.eco <-ecology(taxa, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic"), server = "fishbase")

fb.sp <-species(taxa,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "fishbase")

fb.lar <- larvae(taxa, field = c("Species","PlaceofDevelopment"))

#merge into one data.frame
fb.info <-list(fb.eco, fb.sp, fb.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "fishbase")
fb.info

#ecology() and species() are tables from sealifebase
sl.eco <-ecology(taxa, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic"), server = "sealifebase")

sl.sp <-species(taxa,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "sealifebase")

sl.lar <- larvae(taxa, field = c("Species","PlaceofDevelopment"))

#merge the ecosystem location data into one data.frame
sl.info <-list(sl.eco, sl.sp, sl.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "sealifebase")
sl.info

#add columns for the binding of the data frames
sl.info <- sl.info %>% mutate("Mesopelagic"=NA, "Bathypelagic"=NA, "Abyssopelagic"=NA, "Hadopelagic"=NA)

#combine the two server's information
info <- rbind(sl.info, fb.info)

#change the binary data into characters so that we can then collapse all theses columns into one
info.wtrpos<-info %>% select("Species","Benthic", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic") %>% mutate(Benthic = ifelse(Benthic==1,"benthic", NA),Demersal = ifelse(Demersal==1,"demersal", NA),Pelagic = ifelse(Pelagic==1,"pelagic", NA),Megabenthos = ifelse(Megabenthos==1,"megabenthos", NA),Macrobenthos = ifelse(Macrobenthos==1,"macrobenthos", NA), Meiobenthos = ifelse(Meiobenthos==1,"meiobenthos", NA),Epipelagic = ifelse(Epipelagic==1,"epipelagic", NA),Mesopelagic = ifelse(Mesopelagic==1,"mesopelagic", NA),Bathypelagic = ifelse(Bathypelagic==1,"bathypelagic", NA),Abyssopelagic = ifelse(Abyssopelagic==1,"abyssopelagic", NA),Hadopelagic = ifelse(Hadopelagic==1,"hadopelagic", NA)) %>% unite(adult_water_position, Benthic:Hadopelagic,sep=", ", na.rm = T) %>% select(Species,adult_water_position)

#change the binary data into characters so that we can then collapse all theses columns into one
info.adltmtl <- info %>% select(Species, Mobile, Sessile) %>%mutate(Mobile = ifelse(Mobile==1,"mobile", NA),Sessile = ifelse(Sessile==1,"sessile", NA)) %>% unite(adult_motility, Mobile, Sessile,sep=", ", na.rm = T) %>% select(Species,adult_motility)

#merge all the dataframes into one
info<-info %>% select("Species","server","DemersPelag", "DepthRangeShallow", "DepthRangeDeep", "PlaceofDevelopment")
info <-list(info, info.adltmtl, info.wtrpos) %>% reduce(left_join, by = "Species")

#add source column and taxonomic resolution, change coloumn name for merging purposes, remove the server column
info <-info %>% mutate(DemersPelag_source = ifelse(is.na(DemersPelag),"",server), DemersPelag_taxonomic_resolution = ifelse(is.na(DemersPelag),"","species"), DepthRangeShallow_source = ifelse(is.na(DepthRangeShallow),"",server), 
                       DepthRangeShallow_taxonomic_resolution = ifelse(is.na(DepthRangeShallow),"","species"), DepthRangeDeep_source = ifelse(is.na(DepthRangeDeep),"",server),DepthRangeDeep_taxonomic_resolution = ifelse(is.na(DepthRangeDeep),
                      "","species"),adult_motility_source = ifelse(adult_motility=="","",server),adult_motility_taxonomic_resolution = ifelse(adult_motility=="","","species"),adult_water_position_source = ifelse(adult_water_position=="","",server),
                      adult_water_position_taxonomic_resolution = ifelse(adult_water_position=="","","species"),PlaceofDevelopment_source = ifelse(is.na(PlaceofDevelopment),NA,server),PlaceofDevelopment_taxonomic_resolution = ifelse(is.na(PlaceofDevelopment),
                      NA,"species")) %>% dplyr::rename("Species_fixed"="Species") %>% select(-(server))

#join the trait information to the original data
traits_12su<-left_join(sp_12su,info, by = "LCT") 

#replace the blank spaces with NAs
traits_12su <- as.data.frame(lapply(traits_12su, function(x) { ifelse(x=='', NA, x) }))

#filter so have only one species per row 
traits_12su<-traits_12su %>% select(-(row_names),-(subject)) %>% distinct()
traits_12su

#export csv
write_csv(traits_12su,
          here("Processed_data",
               "traits",
               "traits12su.csv"))

#SEARCH FOR BODY SIZE 
#pull length data from databases
fb_length<-species(taxa, field=c("Species","Length","LTypeMaxM"), server = "fishbase") 
fb_length<-fb_length %>% mutate(server= "fishbase")
sl_length<-species(taxa,field=c("Species","Length","LTypeMaxM"), server = "sealifebase")
sl_length<-sl_length %>% mutate(server = "sealifebase")

#merge
length.data<-rbind(fb_length,sl_length)
length.data

#replace the blank spaces with NAs
length.data<- as.data.frame(lapply(length.data, function(x) { ifelse(x=='', NA, x) }))
length.data

#join the trait information to the current database data 
db_12su <-read_csv(here::here("Processed_data",
                              "traits",
                              "traits12su.csv"))

db_12su_lengths<-left_join(db_12su,length.data, by = "LCT") 

#export csv
write_csv(db_12se_lengths,
          here("Processed_data",
               "traits",
               "traits12su_lengths.csv"))

#Trawl Trait Database ####
#Ensure species names are queriable in FishBase and SealifeBase

sp_trawl <- sp_trawl[sp_trawl$level == 'species',] 
#we'ru just going to get traits for the species we know at the species level (instead of family, genus etc.)

#ensure that species names are character class 
glimpse(sp_trawl) 

#clean up and select data of interest
taxa_trawl <- sp_trawl %>% 
  dplyr::rename_with(~(gsub("X.", "", .))) %>% #fix up column names 
  select("LCT", "class","order","family","genus","species") %>% #select columns of interest
  distinct() #pull only distinct rows

#VALIDATE NAMES
#need to make sure that the names of the fish are correct so that they will pull up information when queried against the database
#validate_names allows us to run the list against the fishbase and sealifebase database to see if our names match any entries
#a list is produced that has NAs where the fish names don't match those in the database
#this only works for specie names
#make one object for each database
trawl_fb <- validate_names(taxa_trawl$species, server = "fishbase") #should this be species or LCT?
trawl_fb
trawl_sl <- validate_names(taxa_trawl$species, server = "sealifebase")
trawl_sl

#alternative approach (don't run if above worked?)
trawl_valid <- validate_names(
  taxa_trawl$species,
  server = getOption("FISHBASE_API", "fishbase"))

#count up NAs and see how much is accounted for
sum(is.na(trawl_fb)) 
sum(is.na(trawl_sl)) 

#more name validation is needed here

#SEARCH FOR TRAITS
#once names are validated, we can now search rFishBase
#collecting together all of the species level data for the fish list
#first we group together the ecosystem location data

#ecology() and species() are tables from rfishbase
taxa <- taxa_trawl$species #change this based on which taxa you are finding traits for 

fb.eco <-ecology(taxa, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic"), server = "fishbase")

fb.sp <-species(taxa,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "fishbase")

fb.lar <- larvae(taxa, field = c("Species","PlaceofDevelopment"))

#merge into one data.frame
fb.info <-list(fb.eco, fb.sp, fb.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "fishbase")
fb.info

#ecology() and species() are tables from sealifebase
sl.eco <-ecology(taxa, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic"), server = "sealifebase")

sl.sp <-species(taxa,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "sealifebase")

sl.lar <- larvae(taxa, field = c("Species","PlaceofDevelopment"))

#merge the ecosystem location data into one data.frame
sl.info <-list(sl.eco, sl.sp, sl.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "sealifebase")
sl.info

#add columns for the binding of the data frames
sl.info <- sl.info %>% mutate("Mesopelagic"=NA, "Bathypelagic"=NA, "Abyssopelagic"=NA, "Hadopelagic"=NA)

#combine the two server's information
info <- rbind(sl.info, fb.info)

#change the binary data into characters so that we can then collapse all theses columns into one
info.wtrpos<-info %>% select("Species","Benthic", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic") %>% mutate(Benthic = ifelse(Benthic==1,"benthic", NA),Demersal = ifelse(Demersal==1,"demersal", NA),Pelagic = ifelse(Pelagic==1,"pelagic", NA),Megabenthos = ifelse(Megabenthos==1,"megabenthos", NA),Macrobenthos = ifelse(Macrobenthos==1,"macrobenthos", NA), Meiobenthos = ifelse(Meiobenthos==1,"meiobenthos", NA),Epipelagic = ifelse(Epipelagic==1,"epipelagic", NA),Mesopelagic = ifelse(Mesopelagic==1,"mesopelagic", NA),Bathypelagic = ifelse(Bathypelagic==1,"bathypelagic", NA),Abyssopelagic = ifelse(Abyssopelagic==1,"abyssopelagic", NA),Hadopelagic = ifelse(Hadopelagic==1,"hadopelagic", NA)) %>% unite(adult_water_position, Benthic:Hadopelagic,sep=", ", na.rm = T) %>% select(Species,adult_water_position)

#change the binary data into characters so that we can then collapse all theses columns into one
info.adltmtl <- info %>% select(Species, Mobile, Sessile) %>%mutate(Mobile = ifelse(Mobile==1,"mobile", NA),Sessile = ifelse(Sessile==1,"sessile", NA)) %>% unite(adult_motility, Mobile, Sessile,sep=", ", na.rm = T) %>% select(Species,adult_motility)

#merge all the dataframes into one
info<-info %>% select("Species","server","DemersPelag", "DepthRangeShallow", "DepthRangeDeep", "PlaceofDevelopment")
info <-list(info, info.adltmtl, info.wtrpos) %>% reduce(left_join, by = "Species")

#add source column and taxonomic resolution, change coloumn name for merging purposes, remove the server column
info <-info %>% mutate(DemersPelag_source = ifelse(is.na(DemersPelag),"",server), DemersPelag_taxonomic_resolution = ifelse(is.na(DemersPelag),"","species"), DepthRangeShallow_source = ifelse(is.na(DepthRangeShallow),"",server), 
                       DepthRangeShallow_taxonomic_resolution = ifelse(is.na(DepthRangeShallow),"","species"), DepthRangeDeep_source = ifelse(is.na(DepthRangeDeep),"",server),DepthRangeDeep_taxonomic_resolution = ifelse(is.na(DepthRangeDeep),
                                                                                                                                                                                                                            "","species"),adult_motility_source = ifelse(adult_motility=="","",server),adult_motility_taxonomic_resolution = ifelse(adult_motility=="","","species"),adult_water_position_source = ifelse(adult_water_position=="","",server),
                       adult_water_position_taxonomic_resolution = ifelse(adult_water_position=="","","species"),PlaceofDevelopment_source = ifelse(is.na(PlaceofDevelopment),NA,server),PlaceofDevelopment_taxonomic_resolution = ifelse(is.na(PlaceofDevelopment),
                                                                                                                                                                                                                                          NA,"species")) %>% dplyr::rename("Species_fixed"="Species") %>% select(-(server))

#join the trait information to the original data
traits_trawl<-left_join(sp_trawl,info, by = "LCT") 

#replace the blank spaces with NAs
traits_trawl <- as.data.frame(lapply(traits_trawl, function(x) { ifelse(x=='', NA, x) }))

#filter so have only one species per row 
traits_trawl<-traits_trawl %>% select(-(row_names),-(subject)) %>% distinct()
traits_trawl

#export csv
write_csv(traits_1trawl,
          here("Processed_data",
               "traits",
               "traitstrawl.csv"))

#SEARCH FOR BODY SIZE 
#pull length data from databases
fb_length<-species(taxa, field=c("Species","Length","LTypeMaxM"), server = "fishbase") 
fb_length<-fb_length %>% mutate(server= "fishbase")
sl_length<-species(taxa,field=c("Species","Length","LTypeMaxM"), server = "sealifebase")
sl_length<-sl_length %>% mutate(server = "sealifebase")

#merge
length.data<-rbind(fb_length,sl_length)
length.data

#replace the blank spaces with NAs
length.data<- as.data.frame(lapply(length.data, function(x) { ifelse(x=='', NA, x) }))
length.data

#join the trait information to the current database data 
db_trawl <-read_csv(here::here("Processed_data",
                              "traits",
                              "traitstrawl.csv"))

db_trawl_lengths<-left_join(db_12su,length.data, by = "LCT") 

#export csv
write_csv(db_trawl_lengths,
          here("Processed_data",
               "traits",
               "traitstrawl_lengths.csv"))


