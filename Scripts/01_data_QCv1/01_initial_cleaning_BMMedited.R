### Goal = dio initial cleaning of data
### Authors = Anya Mueller and Jennifer Sunday 
### Edited by = Tessa Rehill 

#goal: initial cleaning of raw data files and creation of key metatada files 

# Set up ------------------------------------------------------------------
## install packages
# install the following packages if needed
#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("vegan")
#install.packages("here")
#install.packages("biogeo")
#install.packages("lubridate")
#install.packages("readxl")

## read in packages
library(tidyverse)
library(janitor)
library(vegan)
library(here)
library(biogeo)
library(terra)
library(lubridate)
library(readxl)

## read in files
data12Se_asvmatrix <- read.table(here::here("Raw_data",
                                            "eDNA",
                                            "12s",
                                            "12s_e",
                                            "sequence_table.12S.merged.w_ASV_names.length_var.txt"),
                                 h=TRUE)

data12Su_asvmatrix <- read.table(here::here("Raw_data",
                                            "eDNA",
                                            "12s",
                                            "12s_u",
                                            "sequence_table.12S.merged.w_ASV_names.length_var.txt"),
                                 h=TRUE)

co1_metadata <- read.csv(here::here("Raw_data",
                                    "eDNA",
                                    "co1",
                                    "metadata",
                                    "2018_trawl_eDNA_metadata.csv"),
                         head=TRUE)

trawl_metadata <- read.csv(here::here("Raw_data",
                                      "trawl",
                                      "metadata",
                                      "trawl_tow_sample_data.csv"), 
                           head=TRUE, 
                           stringsAsFactors=FALSE, 
                           fileEncoding="latin1")

trawl_catch_sum <- read_xlsx(here("Raw_data", # read in an excel file
                                  "trawl",
                                  "catch_data",
                                  "Nordic_Pearl_Survey_Trawl_Specimen_Log_11_18_20.xlsx"))

trawl_catch <- read_csv(here("Raw_data", 
                             "trawl",
                             "catch_data",
                             "Trawl_catch_data.csv"))
# eDNA metadata -----------------------------------------------------------
#make general eDNA_collection_info dataframe by removing CO1-specific info and duplicate rows

##clean up the co1 metadata and add some columns of interest##

co1_metadata <- co1_metadata %>%
  clean_names(.) %>% #clean up column names
  mutate(depth = as.numeric(str_remove(depth, 
                                       "m"))) %>% #make depth numeric and remove "m" from depth value
  mutate(site = paste(depth,
                      area,
                      set_number, sep = "_")) %>% #make site id based off depth, region, and trawl number
  mutate(north_south = if_else(condition  = str_detect(string = area,
                                                       pattern = "MI|JS"),
                               true = "N", 
                               false = "S")) #add column for north south

## make general eDNA collection info ## 
eDNA_metadata <- co1_metadata %>%
  select(-sample_id, 
         -leray_tag_seq, 
         -leray_tag_id, 
         -pool, 
         -mi_seq_run_id, 
         -pcr_rep) %>% #strip CO1-specific info
  distinct()

## export ##
write_csv(eDNA_metadata,
          here("Processed_data",
               "eDNA",
               "metadata",
               "clean_data",
               "eDNA_metadata.csv")) #saves file to repo 


# 12Se ASV matrix ---------------------------------------------------------------------
## clean up data
#separate row_name label ASV table into vectors of information
data12Se_asvmatrix <- data12Se_asvmatrix %>%
  mutate(row_names=str_replace(row_names, 
                               pattern="Neg", 
                               replacement="tdnaneg-")) %>% #add a dash to the negative controls so that they separate well
  separate(row_names, 
           c("sample_name", 
             "PCR_rep", 
             "marker", 
             "marker_type"), #separate row_name label into vectors of information
           sep="-") %>%
  mutate(unique_id=paste(sample_name, PCR_rep, sep="_"),
         .after = "marker_type" ) #make unique identifier for each row

#join metadata to ASV data
data12Se_asvmatrix_metadata <- full_join(eDNA_metadata,
                                         data12Se_asvmatrix, 
                                         by="sample_name")
#remove controls
data12Se_asvmatrix_metadata_nc <- data12Se_asvmatrix_metadata %>%
  filter(!sample_name %in% c("Neg", 
                             "Pos",
                             "tdnaneg")) %>%
  filter(sample_name != "tdna043") #remove non-sequenced sample

#write out no control dataset
write_csv(data12Se_asvmatrix_metadata_nc,
          here("Processed_data",
               "eDNA",
               "12s", 
               "12s_e",
               "asv",
               "matrix",
               "clean_data",
               "data12Se_asvmatrix_metadata_nc.csv")) #saves file to repo 


# 12Su ASV matrix --------------------------------------------------------------------
## clean up data
#separate row_name label ASV table into vectors of information
data12Su_asvmatrix <- data12Su_asvmatrix %>%
  mutate(row_names=str_replace(row_names, 
                               pattern="NEG", 
                               replacement="tdnaneg")) %>% #add a dash to the negative controls so that they separate well
  separate(row_names, 
           c("sample_name", 
             "PCR_rep", 
             "marker", 
             "marker_type"), #separate row_name label into vectors of information
           sep="-") %>%
  mutate(unique_id=paste(sample_name, PCR_rep, sep="_"),
         .after = "marker_type" ) #make unique identifier for each row

#join metadata to ASV data
data12Su_asvmatrix_metadata <- full_join(eDNA_metadata,
                                         data12Su_asvmatrix, 
                                         by="sample_name")
#remove controls
data12Su_asvmatrix_metadata_nc <- data12Su_asvmatrix_metadata %>%
  filter(!sample_name %in% c("Neg", 
                             "Pos",
                             "tdnaneg")) %>%
  filter(sample_name != "tdna043") #remove non-sequenced sample

#write out no control dataset
write_csv(data12Su_asvmatrix_metadata_nc,
          here("Processed_data",
               "eDNA",
               "12s", 
               "12s_u",
               "asv",
               "matrix",
               "clean_data",
               "data12Su_asvmatrix_metadata_nc.csv")) #writes file to repo 


# Trawl metadata ----------------------------------------------------------
## clean up trawl sample metadta

trawl_metadata <- trawl_metadata %>% 
  #clean up column names
  clean_names() %>%
  #change lat and long with space separators into decimal degree lats and longs
  separate(lat_door_in, 
           c("lat_door_in_deg", "lat_door_in_min"), 
           sep=" ") %>%
  separate(lat_door_out, 
           c("lat_door_out_deg", "lat_door_out_min"), 
           sep=" ") %>%
  separate(long_door_in, 
           c("long_door_in_deg", "long_door_in_min"), 
           sep=" ") %>%
  separate(long_door_out,
           c("long_door_out_deg", "long_door_out_min"), 
           sep=" ") %>%
  mutate(lat_door_in_min = as.numeric(
    trimws(lat_door_in_min)),
    lat_door_out_min = as.numeric(
      trimws(lat_door_out_min)),
    long_door_in_min = as.numeric(
      trimws(long_door_in_min)),
    long_door_out_min = as.numeric(
      trimws(long_door_out_min))) %>%
  mutate(lat_door_in_deg = as.numeric(
    trimws(lat_door_in_deg)),
    lat_door_out_deg = as.numeric(
      trimws(lat_door_out_deg)),
    long_door_in_deg = as.numeric(
      trimws(long_door_in_deg)),
    long_door_out_deg = as.numeric(
      trimws(long_door_out_deg)))  %>%
  mutate(lat_door_in_dd = dms2dd(lat_door_in_deg, 
                                 lat_door_in_min, 
                                 0, 
                                 "N")) %>%
  mutate(long_door_in_dd = dms2dd(long_door_in_deg,
                                  long_door_in_min,
                                  0,
                                  "W")) %>%
  mutate(lat_door_out_dd = dms2dd(lat_door_out_deg, 
                                  lat_door_out_min, 
                                  0, 
                                  "N")) %>%
  mutate(long_door_out_dd = dms2dd(long_door_out_deg, 
                                   long_door_out_min,
                                   0,
                                   "W")) %>%
  mutate(set_number = as.factor(net_tow)) %>%
  #time: make a new column with duration of trawl survey in minutes
  mutate(duration_minutes = as.numeric(
    hm(time_door_out)-hm(time_door_in))/60)

#NOTE:one of the trawls has a duration of 0... need to investigate:

write_csv(trawl_metadata %>% 
            filter(duration_minutes==0),
          here("Processed_data",
               "trawl",
               "metadata",
               "data_exploration",
               "trawl_zero_min_duration.csv"))

## clean up some variables so they match with eDNA data
trawl_metadata <- trawl_metadata %>%
  #clean up area column - replace values so they match eDNA metadata
  mutate(area = str_replace(string = area,
                            pattern = "SJF",
                            replacement = "JF"))

#write out new datafile
write_csv(trawl_metadata, 
          here("Processed_data",
               "trawl",
               "metadata",
               "clean_data",
               "trawl_metadata.csv"))

# Trawl catch data --------------------------------------------------------
## column x13 contains information on taxonomic assingments, lets isolate it 
trawl_catch_name_index <- trawl_catch_sum %>%
  #clean up column names
  clean_names() %>%
  select(x13) %>%
  #remove all the NAs
  drop_na() %>%
  #separate the equivalencies
  separate(col = x13,
           into = c("index",
                    "equals"),
           sep = "=") %>%
  filter(index %in% c("A", 
                      "B",
                      "C")) %>%
  mutate(index = str_replace(string = index,
                             pattern = "A",
                             replacement = "Shrimp A")) %>%
  mutate(index = str_replace(string = index,
                             pattern = "B",
                             replacement = "Shrimp B")) %>%
  mutate(index = str_replace(string = index,
                             pattern = "C",
                             replacement = "Shrimp C")) %>%
  full_join(trawl_catch_sum %>%
               #clean up column names
               clean_names() %>%
               select(x13) %>%
               #remove all the NAs
               drop_na() %>%
               #separate the equivalencies
               separate(col = x13,
                        into = c("index",
                                 "equals"),
                        sep = "=") %>%
               filter(!index %in% c("A", 
                                   "B",
                                   "C")),
            .) %>%
  #remove space in front of larval
  mutate(equals = str_replace(string = equals,
                              pattern = " Larval",
                              replacement = "Larval")) %>%
  # fix incomplete name
  mutate(equals = str_replace(string = equals,
                            pattern = "opalsecent",
                            replacement = "Opalescent Inshore Squid")) %>%
  # fix name 
  mutate(equals = str_replace(string = index,
                            pattern = "Fish Larvae \"A\"",
                            replacement = "Fish Larvae A"))


## clean up trawl catch summary data
trawl_catch_sum <- trawl_catch_sum %>% 
  #clean up column names
  clean_names() %>%
  #remove empty columns
  select(-x11,
         -x12,
         -x13) 

## clean up trawl catch data
trawl_catch <- trawl_catch %>%
  #clean up column names
  clean_names() %>%
  #remove empty columns
  select(-x14,
         -x15) 

## check names in the files against index names

trawl_catch_sum %>% 
  filter(species %in% trawl_catch_name_index$index) %>%
  glimpse() #matches 0 rows

trawl_catch_sum %>% 
  filter(species %in% trawl_catch_name_index$equals) %>%
  glimpse() #matches 14 rows

trawl_catch %>% 
  filter(species %in% trawl_catch_name_index$index) %>%
  glimpse() #matches 236 rows

trawl_catch %>% 
  filter(species %in% trawl_catch_name_index$equals) %>%
  glimpse() #matches 0 rows
#so we want to convert the species names in trawl_catch from the "index" names to the "equals" names

trawl_catch <- trawl_catch %>% 
  #replace names with index names - there is probably a better way to do this...
  mutate(species = str_replace(string = species,
                               pattern = "Squid A",
                               replacement = "Opalescent Inshore Squid")) %>%
  mutate(species = str_replace(string = species,
                               pattern = "Squid C",
                               replacement = "Clawed Armhook")) %>%
  mutate(species = str_replace(string = species,
                               pattern = "Fish Larvae A",
                               replacement = "Larval Sand Lance")) %>%
  mutate(species = str_replace(string = species,
                               pattern = "Shrimp A",
                               replacement = "Neomysis rayii")) %>%
  mutate(species = str_replace(string = species,
                               pattern = "Shrimp B",
                               replacement = "Pasiphaea pacifica")) %>%
  mutate(species = str_replace(string = species,
                               pattern = "Shrimp C",
                               replacement = "Pasiphaea tarda")) %>%
  #separate specific classifiers for the species name ie (juvenile)
  mutate(size_class = if_else(condition = str_detect(string = species,
                                                     pattern = "juvenile"),
                              true = "juvenile",
                              false = NA_character_)) %>%
  #remove juvenile classifier from species name
  mutate(species = str_remove(string = species,
                              pattern = "juvenile")) %>%
  #remove remaining brackets
  mutate(species = str_remove_all(string = species,
                              pattern = "[//(//)]"))
  

## the trawl catch sum is missing catch numbers, lets retrieve them from trawl_catch

trawl_catch_sum <- trawl_catch %>% 
  # count up the unique values
  count(species, trawl, size_class) %>%
  #rename the column
  rename("individs_caught" = "n") %>%
  # join with summary dataframe
  full_join(trawl_catch_sum,
            .)
#NOTE: there is a bit of discrepancy between the two dataframes... not sure which we should trust

## rename some variables for consistency with eDNA data
trawl_catch_sum <- trawl_catch_sum %>%
  rename("set_number" = "trawl")

## export data
write_csv(trawl_catch_sum, 
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_catch_sum.csv"))

write_csv(trawl_catch, 
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_catch.csv"))



