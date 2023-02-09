#Dataset Curation 
#goal: makes datasets for analysis 
#Set-Up 


library(tidyr)
library(tidyverse)
library(here)
library(dplyr)


ASVtaxsample12se <-  read.csv(here::here("Processed_data",
                                                  "eDNA",
                                                   "12s",
                                                  "12s_e",
                                               "eDNAindex",
                              "data12se_taxonomy_index.csv"), #has eDNA index reads
                                               head=TRUE)

ASVtaxsample12se <- select(ASVtaxsample12se, -c("class", "class_read_raw", "class_read_index", "class_pa"))

ASVtaxsample12su <-  read.csv(here::here("Processed_data",
                                         "eDNA",
                                         "12s",
                                         "12s_u",
                                         "eDNAindex",
                                         "data12su_taxonomy_index.csv"), #has eDNA index reads
                              head=TRUE)

trawl_meta <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

eDNA_meta_12su <- read.csv(here::here("Processed_data", 
                                      "eDNA",
                                      "12s",
                                      "12s_u",
                                      "asv",
                                      "matrix",
                                      "clean_data",
                                      "data12Su_asvmatrix_metadata_nc.csv"),
                           head=TRUE)
eDNA_meta_12se <- read.csv(here::here("Processed_data", 
                                      "eDNA",
                                      "12s",
                                      "12s_e",
                                      "asv",
                                      "matrix",
                                      "clean_data",
                                      "data12Se_asvmatrix_metadata_nc.csv"),
                           head=TRUE)

trawl_spp <-  read.csv(here::here("Processed_data",
                                  "trawl",
                                  "catch_data",
                                  "clean_data",
                                  "trawl_sum_cleanFINAL.csv"), 
                       head=TRUE)

#Valid Set Numbers ####

#figure out all set_numbers that have more than 50m differences 
#make new column which is difference between two values 

#Extract relevant samples 
#Select samples within 50m 
#find out which set numbers have greater than 50m between eDNA sample and tral 
trawl <- select(trawl_meta, c("set_number", "depth_mean"))
eDNA <- select(eDNA_meta_12se, c("set_number", "depth"))
depths <- merge(trawl, eDNA, by="set_number")

depths$difference <- abs(depths$depth_mean - depths$depth)

valid_sets <- subset(depths, difference <= 50, #select only difference less than or equal to 50 
                     select=c(set_number, difference))

valid_set_numbers <- unique(valid_sets$set_number)

valid_set_numbers

#figure out all set_numbers that have more than 50m differences 
#make new column which is difference between two values 

#Extract relevant samples

#eDNA dataset ####
#select only species that are present (pa) by subsetting 
#ASVtaxsample12se <- ASVtaxsample12se[ASVtaxsample12se$species_pa != 0,] #not doing this anymore because we want pa for Jaccards
#ASVtaxsample12su <- ASVtaxsample12su[ASVtaxsample12su$species_pa != 0,]

#extract sets only in these set numbers 
eDNA_meta_12se <- eDNA_meta_12se[  eDNA_meta_12se$set_number %in% valid_set_numbers, ] 
eDNA_meta_12su <- eDNA_meta_12su[  eDNA_meta_12su$set_number %in% valid_set_numbers, ] 

valid_samples12se <-  unique(eDNA_meta_12se$original_sample_name)
valid_samples12su <-  unique(eDNA_meta_12se$original_sample_name)

ASVtaxsample12se <- ASVtaxsample12se[  ASVtaxsample12se$original_sample_name %in% valid_samples12se, ] 
ASVtaxsample12su <- ASVtaxsample12su[  ASVtaxsample12su$original_sample_name %in% valid_samples12su, ] 


#Let's add the regional distinction for these samples 

ASVtaxsample12se <- merge(ASVtaxsample12se, eDNA_meta_12se, by="original_sample_name")

ASVtaxsample12su <- merge(ASVtaxsample12su, eDNA_meta_12su, by="original_sample_name")

#subset rows we are interested in 

ASVtaxsample12se <- ASVtaxsample12se[1:37]
ASVtaxsample12su <- ASVtaxsample12su[1:37]

#subset non-present species in eDNA 
ASVtaxsample12se <- subset(ASVtaxsample12se, species_pa == 1)
ASVtaxsample12su <- subset(ASVtaxsample12su, species_pa == 1)

#merge data 
eDNA_df <- rbind(ASVtaxsample12se, ASVtaxsample12su) #merge 12se and 12su 


#replicates are irrelevant, we are going to get rid of columns identifying different replicates 
#remove columns that are irrelevant
eDNA_df <- eDNA_df %>% select(-project_name, -marker, -PCR_rep)


#eDNA_norep <- eDNA_df[1:30]
#eDNA_norep = subset(eDNA_norep, select = -c(PCR_rep) )
#eDNA_norep <- distinct(eDNA_norep)
#eDNA_norep #this is our eDNA dataset using all replicates
#eDNA_norep #this is our eDNA dataset! 

#let's deal with out replicates by aggregating by set number

#x <- eDNA_df

#eDNA_new <- x %>%
#  group_by(set_number, LCT) %>%
#  dplyr::summarise(set_read_count = sum(value))

x <- eDNA_df
eDNA_new <- x %>%
  group_by(set_number, LCT) %>%
  dplyr::summarise(set_read_index = sum(species_read_index)) 


#we want all the other information too! let's merge the previous dataset to our new aggregation 
#let's select the relevant information that we want to keep (can change this later if we want more info in our df)
#north_south
#area 
#site
#marker_type
#set_number (as our key)
#level 

#merge eDNA_new with eDNA_df 

eDNA_df <- merge(eDNA_new, eDNA_df, by=c('set_number', 'LCT'))
eDNA_df <- distinct(eDNA_df)

#not sure if this code is useful anymore
#eDNA_meta <- select(eDNA_df, c('north_south','site', 'set_number', 'marker_type', 'LCT','depth' ))
#eDNA_meta <- distinct(eDNA_meta) #remove duplicates
#eDNA_all <- merge(eDNA_new, eDNA_meta, by.x=c('set_number', 'LCT'), by.y=c('set_number', 'LCT'))
#eDNA_all #this is our dataset! 


#Fixing some species names 
eDNA_df <- data.frame(lapply(eDNA_df, function(x) {
  gsub("Gadus chalcogrammus", "Gadus/chalcogrammus", x) 
  
}))

#eDNA_all <- data.frame(lapply(eDNA_all, function(x) {
#  gsub("Gadus chalcogrammus", "Gadus/chalcogrammus", x) 
  
#}))

write_csv(eDNA_df,
          here("Processed_data",
               "eDNA",
               "datasets",
               "eDNAfulldataset.csv"))

#write_csv(eDNA_all,
#          here("Processed_data",
#               "eDNA",
#               "eDNAfulldatasetmin.csv"))



#Trawl Dataset ####
#we want a dataset with every species caught for each trawl w/ region attached 
#trawl species + set number --> trawl_catch_sum 
#need to harmonize taxonomy of this dataset (done in trawltaxonomy script )


#parse out relevant set numbers 
trawl_spp <- trawl_spp[  trawl_spp$set_number %in% valid_set_numbers, ] 
trawl_meta <- trawl_meta[  trawl_meta$set_number %in% valid_set_numbers, ] 
#then add regional information from trawl_meta 

trawl_df <- merge(trawl_spp, trawl_meta, by= "set_number") #has LCT

names(trawl_df)[names(trawl_df) == "leg.y"] <- "north_south" #rename region column (southern/northern)

trawl_df #this is our species data set 

#remove Sebastes species 
trawl_df <- subset(trawl_df, LCT != 'Sebastes sp')

#trawl_df_min <- select(trawl_df, c('set_number','common_name', 'LCT','area','region', 'depth_mean', 'individs_caught' ))


#writing files 

write_csv(trawl_df,
          here("Processed_data",
               "trawl",
               "datasets",
               "fulldatasettrawl.csv")) 

#write_csv(trawl_df_min,
#          here("Processed_data",
#               "trawl",
#               "fulldatasettrawlmin.csv")) 


#add weight to trawl catch 

trawl_ind <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "catch_data",
                                  "clean_data",
                                  "trawl_all_cleanFINAL.csv"),
                       head=TRUE) #has LCT 

#get rid of NA values in weight column 

trawl_ind[is.na(trawl_ind$weight_kg),] <- 0


x <- trawl_ind

trawl_weight <- x %>%
  group_by(trawl, LCT) %>%
  dplyr::summarise(weight_total_kg = sum(weight_kg)) 

#rename column names 
colnames(trawl_weight) <- c('set_number', 'LCT' ,'weight_total_kg')

#add weight to full trawl dataset 
all <- merge(trawl_df, trawl_weight, by=c('set_number', 'LCT')) #this is where we LOSE the LCT

write_csv(all,
          here("Processed_data",
               "trawl",
               "datasets",
               "fulldatasettrawlweight.csv")) 

###Notes: when you go into the raw data, it says the following individuals were discarded
#or their weight was not taken 

#1. Sebastes sp. 
#2. Mallotus villosus
#3. Corphaenidoes sp - had notes that it weighed less than 0.002 

#manually inserted weights for these species as follows so that they are still considered 'present'
#Sebastes sp. + Mallotus villosus = 0.0001
#Corphaenidoes sp = 0.002

