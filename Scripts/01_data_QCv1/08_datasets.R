#Dataset Curation 
#goal: makes datasets for analysis 

#Set-Up ####
#read in packages
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)

#read in files
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
                                  "trawl_sum_clean.csv"), 
                       head=TRUE)

#Valid Set Numbers #### (valid set number = all sets)
#originally we did not include some sets 
#make new column which is difference between sampling depths 

trawl <- select(trawl_meta, c("set_number", "depth_mean")) #select relevant columns 
eDNA <- select(eDNA_meta_12se, c("set_number", "depth"))
depths <- merge(trawl, eDNA, by="set_number") #merge depth info

depths$difference <- abs(depths$depth_mean - depths$depth) #make new column 

valid_sets <- subset(depths, difference <= 1000, #select only difference less than or equal to 'difference =' 
                     select=c(set_number, difference))

valid_set_numbers <- unique(valid_sets$set_number)

valid_set_numbers #should be 1-16


#extract sets only in these set numbers  #not relevant anymore since we are keeping all sets despite set number
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
eDNA_df <- eDNA_df %>% select(-project_name, -marker_type, -PCR_rep)


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


#Fixing some species names 
eDNA_df <- data.frame(lapply(eDNA_df, function(x) {
  gsub("Gadus chalcogrammus", "Gadus/chalcogrammus", x) 
  
}))


write_csv(eDNA_df,
          here("Processed_data",
               "eDNA",
               "datasets",
               "eDNA_allsets.csv"))



#Trawl Dataset
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

#writing files 

write_csv(trawl_df,
          here("Processed_data",
               "trawl",
               "datasets",
               "trawl_allsets.csv")) 

#add weight to trawl catch 

trawl_ind <- read.csv(here::here("Processed_data", 
                                 "trawl",
                                 "catch_data",
                                 "clean_data",
                                 "trawl_catch_clean.csv"),
                      head=TRUE) 

#go into data + add weight for Corphaenidoes sp (look at notes)
#

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
               "trawlweight_allsets.csv")) 
