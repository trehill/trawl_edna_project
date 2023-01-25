#Dataset Curation 
#goal: makes datasets for analysis 
#Set-Up 


library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(dplyr)


ASVtaxsample12se <-  read.csv(here::here("Processed_data",
                                       "eDNA",
                                       "12s",
                                       "12s_e",
                                       "LCTassignment",
                                       "ASVtaxonomybysample12se.csv"),
                                  head=TRUE)

ASVtaxsample12su <- read.csv(here::here("Processed_data",
                                        "eDNA",
                                        "12s",
                                        "12s_u",
                                        "LCTassignment",
                                        "ASVtaxonomybysample12su.csv"),
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


#Valid Set Numbers ####
#figure out all set_numbers that have more than 50m differences 
#make new column which is difference between two values 

#Extract relevant samples ####
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

#eDNA dataset ####
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

ASVtaxsample12se <- ASVtaxsample12se[1:31]
ASVtaxsample12su <- ASVtaxsample12su[1:31]

eDNA_df <- rbind(ASVtaxsample12se, ASVtaxsample12su) #merge 12se and 12su 

#Here is where we will deal with replicates (right now there are duplicated lines because the replicates are there... )
#For now we are gonna get rid of replicates, and just remove duplicated rows 

write_csv(eDNA_df,
          here("Processed_data",
               "eDNA",
               "eDNAfulldataset.csv"))

#write_csv(eDNA_df, "eDNAfulldataset.csv") #write csv function not working 

#eDNA_norep <- eDNA_df[1:30]
#eDNA_norep = subset(eDNA_norep, select = -c(PCR_rep) )
#eDNA_norep <- distinct(eDNA_norep)
#eDNA_norep #this is our eDNA dataset using all replicates
#eDNA_norep #this is our eDNA dataset! 

#let's deal with out replicates by aggregating by set number

x <- eDNA_df

eDNA_new <- x %>%
  group_by(set_number, LCT) %>%
  dplyr::summarise(set_read_count = sum(value))



#we want all the other information too! let's merge the previous dataset to our new aggregation 
#let's select the relevant information that we want to keep (can change this later if we want more info in our df)
#north_south
#area 
#site
#marker_type
#set_number (as our key)
#level 

eDNA_meta <- select(eDNA_df, c('north_south','area', 'set_number', 'marker_type', 'LCT','level' ))
eDNA_meta <- distinct(eDNA_meta) #remove duplicates

eDNA_all <- merge(eDNA_new, eDNA_meta, by.x=c('set_number', 'LCT'), by.y=c('set_number', 'LCT'))

eDNA_all #this is our dataset! 

write_csv(eDNA_all,
          here("Processed_data",
               "eDNA",
               "eDNAfulldatasetmin.csv"))

#write_csv(eDNA_all, "eDNAfulldatasetmin.csv") #write csv function not working 

#Trawl Dataset... this might not be as easy ####
#we want a dataset with every species caught for each trawl w/ region attached 
#trawl species + set number --> trawl_catch_sum 
#need to harmonize taxonomy of this dataset (done in trawltaxonomy script )

trawl_spp <- read.csv(here:here("Processed_data",
                                "trawl",
                                "catch_data",
                                "clean_data",
                                "trawl_sum_clean.csv"),
                      head=TRUE)

trawl_spp <- final_trawl #since write_csv function is acting weird 

#parse out relevant set numbers 
trawl_spp <- trawl_spp[  trawl_spp$set_number %in% valid_set_numbers, ] 
trawl_meta <- trawl_meta[  trawl_meta$set_number %in% valid_set_numbers, ] 
#then add regional information from trawl_meta 

trawl_df <- merge(trawl_spp, trawl_meta, by= "set_number")

names(trawl_df)[names(trawl_df) == "leg.y"] <- "region" #rename region column (southern/northern)

trawl_df #this is our species data set 

write_csv(trawl_df,
          here("Processed_data",
               "trawl",
               "catch_data",
               "fulldatasettrawl.csv")) #also not working? 

#write_csv(trawl_df, "fulldatasettrawl.csv") #write csv function not working 

trawl_df_min <- select(trawl_df, c('set_number','common_name', 'species', 'level','area','region' ))

write_csv(trawl_df_min,
          here("Processed_data",
               "trawl",
               "catch_data",
               "fulldatasettrawlmin.csv")) #also not working? 

#write_csv(trawl_df_min, "fulldatasettrawlmin.csv") #write csv function not working 
