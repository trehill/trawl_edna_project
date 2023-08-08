#04 field control removal 
#####goal: remove contaminants from sample read numbers according to maximum concentration in negative controls
#####in the day (or control group) when it was detected
#in long format, associate date of collection from metadata
#group by samples taken on the day on or before the negative control
#make new column for each ASV that is max read in that control group
#subtract from reads in samples, remove ASVs with zero or lower

## read in packages
#install.packages("ggVennDiagram")

library(tidyverse)
library(janitor)
library(here)
library(ggVennDiagram)
library("lubridate")

#read in metadata
metadata_trawl<- read_csv(here::here("Processed_data",
                                     "eDNA",
                                     "metadata",
                                     "clean_data",
                                     "eDNA_metadata.csv"))

# 12se --------------------------------------------------------------------
# read in files
# ASV by sites matrix after removal of false postives via occupancy modelling. NOTE: location of this file will change
data12Se_asvmatrix <- read_csv(here::here("Processed_data",
                                          "eDNA",
                                          "12s",
                                          "12s_e",
                                          "asv",
                                          "matrix",
                                          "clean_data",
                                          "data12se_asvmatrix_nc_lor.csv"))
ncol(data12Se_asvmatrix)
#join with meta_data
data12Se_asvmatrix_meta<-left_join(metadata_trawl, rename(data12Se_asvmatrix, "sample_name"="...1"), by="sample_name")

#get a vector of date-times when controls were taken
control_times <- data12Se_asvmatrix_meta %>% #start with meta_data and matrix
  select(original_sample_name, type, date, time_of_filter) %>% #select info needed
  filter(type=="control") %>% #just get controls
  mutate(date_time=ymd(date)+hms(time_of_filter)) %>% #lubridate commands that allow dates and times to be vectors
  select(date_time) #just keep this column

#make this into an object, and add times before and after so that all samples will be in a group
control_time_breaks<-control_times$date_time %>%
  append("2018-07-02 12:50:00 UTC") %>% #add a much lower time
  append("2018-09-13 08:42:00 UTC") #add a much later time


#here is the QC step to remove negative controls according to daily controls
data12Se_asvmatrix_long_daily_fieldcontrols<-data12Se_asvmatrix_meta %>% #start with meta_data and matrix
  select(original_sample_name, type, date, time_of_filter, contains("ASV")) %>% #select info needed
  gather("ASV", "read", -original_sample_name, -type, -date, -time_of_filter) %>% #make data long
  drop_na("read") %>% # get rid of NAs in the read column - this removes negative/positive controls and sample with no data
  mutate(date_time=ymd(date)+hms(time_of_filter)) %>% 
  mutate(control_group = cut(date_time,  #create control groups
                             breaks=control_time_breaks, #according to time of controls
                             right=T)) %>% #set break so that all reads before control are part of its group
  group_by(control_group, ASV) %>% #group data according to control groups and ASVs
  mutate(control_read = read[which(type=="control")])  %>% #create a new column with control read for each control group x ASV combo
  ungroup() %>%
  mutate(read_controls_rmd = (read-control_read)) #subtract control reads
#output has lots of columns that shows what was substracted from which control group X ASV combo

#clean up output to just the reads above 0 are kept, and just useful columns
data12Se_nolowocc_nofieldcontrols<-data12Se_asvmatrix_long_daily_fieldcontrols %>%
  filter(read_controls_rmd>0) %>% #remove rows with read of 0 or lower
  select(original_sample_name, ASV, read_controls_rmd) # removes some columns

#make data wide incase people prefer the matrix format
data12Se_asvmatrix_nolowocc_nofieldcontrols<-data12Se_nolowocc_nofieldcontrols %>% 
  spread(ASV, read_controls_rmd) 
#this leaves NAs throughout table instead of 0s; replace NAs with 0
data12Se_asvmatrix_nolowocc_nofieldcontrols[is.na(data12Se_asvmatrix_nolowocc_nofieldcontrols)] <- 0 #replace NAs with 0



#write out the new data files
write_csv(data12Se_asvmatrix_nolowocc_nofieldcontrols, 
          here("Processed_data","eDNA","12s", "12s_e", "asv", "matrix", "clean_data",
               "data12Se_asvmatrix_nc_lor_nfc.csv"))

write_csv(data12Se_long_nolowocc_nofieldcontrols_taxa, 
          here("Processed_data","eDNA","12s", "12s_e", "asv", "matrix", "clean_data",
               "data12Se_asv_taxonomy_long_nc_lor_nfc.csv"))

# 12su --------------------------------------------------------------------

# read in files
# ASV by sites matrix after removal of false postives via occupancy modelling. NOTE: location of this file will change
data12Su_asvmatrix <- read_csv(here::here("Processed_data",
                                          "eDNA",
                                          "12s",
                                          "12s_u",
                                          "asv",
                                          "matrix",
                                          "clean_data",
                                          "data12su_asvmatrix_nc_lor.csv"))

ncol(data12Su_asvmatrix)

#join with meta_data
data12Su_asvmatrix_meta<-left_join(metadata_trawl, rename(data12Su_asvmatrix, "sample_name"="...1"), by="sample_name")

#get a vector of date-times when controls were taken
control_times <- data12Su_asvmatrix_meta %>% #start with meta_data and matrix
  select(original_sample_name, type, date, time_of_filter) %>% #select info needed
  filter(type=="control") %>% #just get controls
  mutate(date_time=ymd(date)+hms(time_of_filter)) %>% #lubridate commands that allow dates and times to be vectors
  select(date_time) #just keep this column

#make this into an object, and add times before and after so that all samples will be in a group
control_time_breaks<-control_times$date_time %>%
  append("2018-07-02 12:50:00 UTC") %>% #add a much lower time
  append("2018-09-13 08:42:00 UTC") #add a much later time


#here is the QC step to remove negative controls according to daily controls
data12Su_asvmatrix_long_daily_fieldcontrols<-data12Su_asvmatrix_meta %>% #start with meta_data and matrix
  select(original_sample_name, type, date, time_of_filter, contains("ASV")) %>% #select info needed
  gather("ASV", "read", -original_sample_name, -type, -date, -time_of_filter) %>% #make data long
  drop_na("read") %>% # get rid of NAs in the read column - this removes negative/positive controls and sample with no data
  mutate(date_time=ymd(date)+hms(time_of_filter)) %>% 
  mutate(control_group = cut(date_time,  #create control groups
                             breaks=control_time_breaks, #according to time of controls
                             right=T)) %>% #set break so that all reads before control are part of its group
  group_by(control_group, ASV) %>% #group data according to control groups and ASVs
  mutate(control_read = read[which(type=="control")])  %>% #create a new column with control read for each control group x ASV combo
  ungroup() %>%
  mutate(read_controls_rmd = (read-control_read)) #subtract control reads
#output has lots of columns that shows what was substracted from which control group X ASV combo

#clean up output to just the reads above 0 are kept, and just useful columns
data12Su_nolowocc_nofieldcontrols<-data12Su_asvmatrix_long_daily_fieldcontrols %>%
  filter(read_controls_rmd>0) %>% #remove rows with read of 0 or lower
  select(original_sample_name, ASV, read_controls_rmd) # removes some columns

#make data wide incase people prefer the matrix format
data12Su_asvmatrix_nolowocc_nofieldcontrols<-data12Su_nolowocc_nofieldcontrols %>% 
  spread(ASV, read_controls_rmd) 
#this leaves NAs throughout table instead of 0s; replace NAs with 0
data12Su_asvmatrix_nolowocc_nofieldcontrols[is.na(data12Su_asvmatrix_nolowocc_nofieldcontrols)] <- 0 #replace NAs with 0


#write out the new data files
write_csv(data12Su_asvmatrix_nolowocc_nofieldcontrols, 
          here("Processed_data","eDNA","12s", "12s_u", "asv", "matrix", "clean_data",
               "data12Su_asvmatrix_nc_lor_nfc.csv"))
