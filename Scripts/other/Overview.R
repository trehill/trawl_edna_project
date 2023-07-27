#Supplemental Overview Table 
#make table that includes the LCT, detection method (at gamma level), incidence, and mean biomass 

#Set-Up ####
#load libraries 
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(rempsyc)


#read in files 
det <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "diversity",
                            "gamma_spp_count.csv"), #file that contains the incidence of each LCT 
                 head=TRUE)                         #and which detection method (at gamma level)


bio <- read.csv(here::here("Processed_data", 
                           "datasets",
                           "biomass",
                           "species_biomass_mean_all.csv"), #file that contains the final mean biomass
                head=TRUE)                        

edna <- read.csv(here::here("Processed_data", 
                           "eDNA",
                           "datasets",
                           "eDNA_allsets.csv"), #file that contains only eDNA data
                head=TRUE)                        

trawl <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "datasets",
                            "trawlweight_allsets.csv"), #file that contains only eDNA data
                 head=TRUE)                        


#Format Data #### 
#merge two datasets so we have a df with LIT, detection, incidence, mean biomass 

#select relevant columns from each df 
deta <- select(det, c('LCT','n','gamma_detection_method')) #select columns 
bioa <- select(bio, c('LCT','mean_biomass')) #select columns 

#merge 
df <- merge(deta, bioa, by=c('LCT'), all.x=TRUE)
df <- distinct(df) #remove replicates

#arrange data in decreasing order of incidence 
df <- df %>% arrange(n) #arrange numbers in increasing order 

#rename columns 
colnames(df) <- c('LIT','Incidence','Detection', 'Mean Biomass (kg/km)') #change column names 

#Make summary table ####
my_table <- nice_table(
  df[1:40, ], 
  title = c("Table 2", "Overview of Species Detections"), 
  note = c("Mean biomass is not recorded for species detected only in eDNA as this information is not available using this method"))

my_table

#save dataframe 
write_csv(df,
          here("Processed_data",
               "datasets",
               "overview.csv"))

#Determining most common species detected across either method ####
#count
x <- edna

count_spp_edna <- x %>% 
  count(LCT) #count how many times a species is seen across the dataset 

#most detected species in eDNA = clupea palaasi
x <- trawl

count_spp_trawl <- x %>% 
  count(LCT) #count how many times a species is seen across the dataset 



