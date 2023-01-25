#Author: Tessa Rehill 
#goal: assigns ASV to sample and LCT 

#set-up

library(tidyr)
library(reshape2)

#12se ####
sampleASV12se <- read.csv(here::here("Processed_data", #should be ASV by sample
                                    "eDNA",
                                    "12s",
                                    "12s_e",
                                    "asv",
                                    "matrix",
                                    "clean_data",
                                    "data12Se_asvmatrix_nc_lor_nfc.csv"),
                         head=TRUE)


taxa <-  read.csv(here::here("Processed_data", #should be ASV by sample
                             "eDNA",
                             "12s",
                             "12s_e",
                             "LCTassignment",
                             "ASV_taxonomy_12seDNA.csv"),
                  head=TRUE)


data_long <- melt(sampleASV12se, id.vars=c("original_sample_name"))

#remove all rows with value 0 
data_long <- data_long[data_long$value != 0, ]
names(data_long)[2] <- "ASV"

#match to taxonomy by merging
taxASVsample <- merge(data_long, taxa, by="ASV")

write_csv(taxASVsample,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "LCTassignment"
               "ASVtaxonomybysample12se.csv"))

#12su 
sampleASV12su <- read.csv(here::here("Processed_data", #should be ASV by sample
                                     "eDNA",
                                     "12s",
                                     "12s_u",
                                     "asv",
                                     "matrix",
                                     "clean_data",
                                     "data12Su_asvmatrix_nc_lor_nfc.csv"),
                          head=TRUE)


taxa <-  read.csv(here::here("Processed_data", #should be ASV by sample
                             "eDNA",
                             "12s",
                             "12s_u",
                             "LCTassignment",
                             "ASV_taxonomy_12suDNA.csv"),
                  head=TRUE)


data_long <- melt(sampleASV12su, id.vars=c("original_sample_name"))

#remove all rows with value 0 
data_long <- data_long[data_long$value != 0, ]
names(data_long)[2] <- "ASV"

#match to taxonomy by merging
taxASVsample <- merge(data_long, taxa, by="ASV")

write_csv(taxASVsample,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "LCTassignment",
               "ASVtaxonomybysample12su.csv"))
