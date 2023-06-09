#Dataset Curation 
#goal: makes 3 datasets for analysis 
#sum read index's
#sum and add weight measurement column
#harmonize column names across datasets

#Old: extract only data from relevant set numbers (we include all sets 1-16 in our analysis)
#this code has been moved to the bottom of script if needed later


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

eDNA_meta_12su <- eDNA_meta_12su[1:21] #we are not interested in the ASVs
#subset out only relevant columns 
eDNA_meta_12se <- read.csv(here::here("Processed_data", 
                                      "eDNA",
                                      "12s",
                                      "12s_e",
                                      "asv",
                                      "matrix",
                                      "clean_data",
                                      "data12Se_asvmatrix_metadata_nc.csv"),
                           head=TRUE)

eDNA_meta_12se <- eDNA_meta_12se[1:21] #we are not interested in the ASVs
#subset out only relevant columns 

trawl_spp <-  read.csv(here::here("Processed_data",
                                  "trawl",
                                  "catch_data",
                                  "clean_data",
                                  "trawl_sum_clean.csv"), 
                       head=TRUE)


#eDNA dataset ####


#Let's adding metadata for these samples 
ASVtaxsample12se <- merge(ASVtaxsample12se, eDNA_meta_12se, by="original_sample_name", all.x=TRUE)
#increases ASVtaxsample12se observations by three because there are three replicates in eDNA 

ASVtaxsample12su <- merge(ASVtaxsample12su, eDNA_meta_12su, by="original_sample_name", all.x=TRUE)
#increases ASVtaxsample12su observations by three because there are three replicates in eDNA 

colnames(ASVtaxsample12su)
colnames(ASVtaxsample12se)

#these two above dataframes have different #of columns 
#different columns (in 12se but not 12su) refer to class data
#remove class columns from 12se data 

ASVtaxsample12se<- select(ASVtaxsample12se, -c('class','class_read_raw', 'class_read_index', 'class_pa'))

#subset only valid set numbers (all sets, not TS1 or TS2)

ASVtaxsample12se <- subset(ASVtaxsample12se, set_number != c('TS1', 'TS2'))
ASVtaxsample12su <- subset(ASVtaxsample12su, set_number != c('TS1', 'TS2'))

#make set_number column numeric

ASVtaxsample12se$set_number <- as.numeric(ASVtaxsample12se$set_number)
ASVtaxsample12su$set_number <- as.numeric(ASVtaxsample12su$set_number)

#subset only present species in eDNA 
ASVtaxsample12se <- subset(ASVtaxsample12se, species_pa == 1)
ASVtaxsample12su <- subset(ASVtaxsample12su, species_pa == 1)

#merge data, combined 12se/12su together (we are treating eDNA as all 12s)
eDNA_df <- rbind(ASVtaxsample12se, ASVtaxsample12su) #merge 12se and 12su 

#replicates are irrelevant to this analysis, we are going to get rid of columns identifying different replicates 
#remove columns that are irrelevant
eDNA_df <- eDNA_df %>% select(-project_name, -marker_type, -PCR_rep)

#sum read index per set per species 
x <- eDNA_df

eDNA_new <- x %>%
  group_by(set_number, LCT) %>%
  dplyr::summarise(set_read_index = sum(species_read_index)) 


#merge eDNA_new with eDNA_df 
eDNA_df <- merge(eDNA_new, eDNA_df, by=c('set_number', 'LCT'))
eDNA_df <- distinct(eDNA_df) #this gets rid of the many rows per spp that were due to replicates 


#Fixing some species names that were a problem in the trawl 
eDNA_df <- data.frame(lapply(eDNA_df, function(x) {
  gsub("Gadus chalcogrammus", "Gadus/chalcogrammus", x)  #to match trawl dataset 
  
})) 

#Renaming some column names so they match our trawl dataset 
#editing eDNA dataset to have harmonized column names 

eDNA_df <- eDNA_df %>% #rename depth column 
  rename(
    depth_eDNA = depth)

eDNA_df <- eDNA_df %>% #rename presence/absence column 
  rename(
    pabs_eDNA = species_pa)

#Remove species_pa = NA and index = NA, remove NA values 
eDNA_df <- eDNA_df[!is.na(eDNA_df$pabs_eDNA),]
eDNA_df <- eDNA_df[!is.na(eDNA_df$set_read_index),]


write_csv(eDNA_df,
          here("Processed_data",
               "eDNA",
               "datasets",
               "eDNA_allsets.csv"))

#Trawl Dataset
#we want a dataset with every species caught for each trawl w/ region attached 
#trawl species + set number --> trawl_catch_sum 

#Add metadata to trawl dataset

trawl_df <- merge(trawl_spp, trawl_meta, by= "set_number", all.x=TRUE) #has LCT

#Rename column names 
names(trawl_df)[names(trawl_df) == "leg.y"] <- "north_south" #rename region column (southern/northern)

trawl_df <- trawl_df %>% rename(depth_trawl = depth_mean) #rename depth column 

#Add presence column 
trawl_df <- cbind(trawl_df, pabs_trawl = 1) #add column with value 1 to indicate presence/absence in trawl df 

#Rename S and N across dataset
trawl_df <- data.frame(lapply(trawl_df, function(x) { #change southern to S across all df
  gsub("southern", "S", x)
  
}))

trawl_df <- data.frame(lapply(trawl_df, function(x) { #change northern to N across all df
  gsub("northern", "N", x)
  
}))

trawl_df #this is our species data set 

#writing files 

write_csv(trawl_df,
          here("Processed_data",
               "trawl",
               "datasets",
               "trawl_allsets.csv")) 

#Trawl weight dataset ####

trawl_ind <- read.csv(here::here("Processed_data", 
                                 "trawl",
                                 "catch_data",
                                 "clean_data",
                                 "trawl_catch_clean.csv"),
                      head=TRUE) 

#not all our fish in the trawl have weight measurements (instead say NA)
#trying to convert NA in weight to weight measurements from length-weight relationships 
#(1) which species have NA values in the trawl 

is_na <- trawl_ind[is.na(trawl_ind$weight_kg),]
unique(is_na$LCT)
  #Mallotus villosus #has length-weight relationship, only 1 spp. in trawl 
  #Thaleichthus. pacificus #no length-weight relationship, more than 1 spp. in trawl -NA=0
  #Ammodytes hexapterus #has length-weight relationship
  #Diaphus theta #no length-weight relationship, more than 1 spp. in trawl, NA=0
  #Cupea pallasii #has length-weight relationship, more than 1 spp. in trawl, convert NA 
  #Oncorhynchus nerka #has length-weight relationship, no length measurement in dataset, NA=0 
  #Gadus clacogrammus #has length-weight relationship
  #Zoarcidae sp. #not to spp level so not able to find length-weight relationship, but other ind in trawl have measurement, NA=0 

#note: if sp do NOT have length-weight conversion but DO have other spp with weight measurement in trawl, NA=0 
        #this does not effect analysis because during the creation of weight density, weight is SUMMED 

#Let's see if these spp have length weight conversions in FishBase 
#How to interpret these: https://www.fishbase.se/manual/english/fishbasethe_length_weight_table.htm
  #some notes: these conversions are done in cm + g, our measurements are in kg
                #W = a × L^b


#Species 1: Mallotus villosus
#https://fishbase.se/popdyn/LWRelationshipList.php?ID=252&GenusName=Mallotus&SpeciesName=villosus&fc=80
#FL to weight, this species has fork length (cm) in trawl catch dataset
#used Canadian data
#W (g) = 0.00077*FL^3.770, convert to kg 

weight_mv<- subset(is_na, species == 'Mallotus villosus')
weight_mv$weight_kg <- (0.00077*weight_mv$length_cm^3.770)/1000

#Species 2: Ammodytes hexapterus
#https://fishbase.se/popdyn/LWRelationshipList.php?ID=3822&GenusName=Ammodytes&SpeciesName=hexapterus&fc=402
#TL to weight, this species has total length (cm) in trawl catch dataset
#W (g) = 0.00099*TL^3.467, convert to kg 

weight_ah<- subset(is_na, species == 'Ammodytes hexapterus')
weight_ah$weight_kg <- (0.00099*weight_ah$length_cm^3.467)/1000

#Species 3: Clupea pallassi
#https://fishbase.se/popdyn/LWRelationshipList.php?ID=1520&GenusName=Clupea&SpeciesName=pallasii&fc=43
#Northwest territories data, unclear whether should use female or male data since we don't know sex 
#FL to weight, this species has fork length (cm) in trawl catch dataset
#W (g) = 0.000317*TL^3.374, convert to kg 

weight_cp<- subset(is_na, species == 'Clupea pallasii')
weight_cp$weight_kg <- (0.000317*weight_cp$length_cm^3.374)/1000

#Species 4: Oncorhynchus nerka 
#https://fishbase.se/popdyn/LWRelationshipList.php?ID=243&GenusName=Oncorhynchus&SpeciesName=nerka&fc=76
#TL to weight, this species has fork length (cm) in trawl catch dataset
#W (g) = 0.000317*TL^3.374, convert to kg 

weight_on<- subset(is_na, species == 'Oncorhynchus nerka')
  #no weight measurements available - sample was discarded during subsampling, we will give weight = 0 
weight_on$weight_kg <- 0


#Species 5: Gadus clacogrammus
#https://fishbase.se/popdyn/LWRelationshipList.php?ID=318&GenusName=Gadus&SpeciesName=chalcogrammus&fc=183
#TL to weight, this species has total length (cm) in trawl catch dataset
#W (g) = 0.00750*TL^2.977, convert to kg 

weight_gc<- subset(is_na, species == 'Gadus/chalcogrammus')
weight_gc$weight_kg <- (0.00750*weight_gc$length_cm^2.977)/1000

#Species 6: Zoarcidae sp. 
#unable to find length-weight relationships because we can not identify these fish to the sp level 
#NA = 0 

weight_z<- subset(is_na, LCT == 'Zoarcidae sp')
weight_z$weight_kg <- 0

#Species 7: Thaleichthys pacificus
#no weight-length relationship, but other indv. with weight measurements present
weight_tp<- subset(is_na, LCT == 'Thaleichthys pacificus')
weight_tp$weight_kg <- 0

#Species 8: Diaphus theta
#no weight-length relationship, but other indv. with weight measurements present
weight_dt<- subset(is_na, LCT == 'Diaphus theta')
weight_dt$weight_kg <- 0

#bind all these rows (that had NA but have now been converted)
weight_na <- rbind(weight_ah, weight_cp, weight_gc, weight_mv, weight_on, weight_z, weight_tp, weight_dt)

#Take trawl_ind dataset and remove NA, we will then add back the NA species with the new weight measurements
#Both trawl_ind and trawl_new should have the same number of rows 

trawl_new <- trawl_ind[!is.na(trawl_ind$weight_kg),] #remove all rows with NA in weight column 
trawl_new <- rbind(trawl_new, weight_na)

#trawl_new + trawl_ind have same number of rows 
#check if trawl_new contains NA in weight column 

unique(trawl_new$weight_kg) #no NA!

#Add summed weight column per species 
x <- trawl_new

trawl_weight <- x %>%
  group_by(trawl, LCT) %>%
  dplyr::summarise(weight_total_kg = sum(weight_kg)) 

#rename column names 
colnames(trawl_weight) <- c('set_number', 'LCT' ,'weight_total_kg')

#add weight to full trawl dataset 
all <- merge(trawl_df, trawl_weight, by=c('set_number', 'LCT'), all.x=TRUE) 

write_csv(all,
          here("Processed_data",
               "trawl",
               "datasets",
               "trawlweight_allsets.csv")) 


#Set-Numbers (old-code) ####
#Valid Set Numbers #### (valid set number = all sets)
#originally we did not include some sets 
#make new column which is difference between sampling depths 

#trawl <- select(trawl_meta, c("set_number", "depth_mean")) #select relevant columns 
#eDNA <- select(eDNA_meta_12se, c("set_number", "depth"))
#depths <- merge(trawl, eDNA, by="set_number") #merge depth info

#depths$difference <- abs(depths$depth_mean - depths$depth) #make new column 

#valid_sets <- subset(depths, difference <= 1000, #select only difference less than or equal to 'difference =' 
#                     select=c(set_number, difference))

#valid_set_numbers <- unique(valid_sets$set_number)

