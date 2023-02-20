#Gamma Diversity Analysis
#Output: Euler Plot 
#Author: Tessa Rehill 

#Set-up 
#install.packages('ggvenn')
#install.packages('RColorBrewer')
#install.packages('eulerr')

library(dplyr)
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)

#Set-Up ####



#Format datasets ####
#Remove all samples >50m #this could be integrated into data cleaning files ####
#need to find where ASV correlate to samples 

trawl_meta <- read.csv(here::here("Processed_data", #should be ASV by sample
                               "trawl",
                               "metadata",
                               "clean_data",
                               "trawl_metadata.csv"),
                    head=TRUE)

eDNA_meta_12su <- read.csv(here::here("Processed_data", #should be ASV by sample
                                      "eDNA",
                                      "12s",
                                      "12s_u",
                                      "asv",
                                      "matrix",
                                      "clean_data",
                                      "data12Su_asvmatrix_metadata_nc.csv"),
                           head=TRUE)
eDNA_meta_12se <- read.csv(here::here("Processed_data", #should be ASV by sample
                                      "eDNA",
                                      "12s",
                                      "12s_e",
                                      "asv",
                                      "matrix",
                                      "clean_data",
                                      "data12Se_asvmatrix_metadata_nc.csv"),
                           head=TRUE)

ASVtax12se <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "eDNA",
                                  "12s",
                                  "12s_e",
                                  "ASV_taxonomy_12seDNA.csv"),
                       head=TRUE)

sampleASV12se <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "eDNA",
                                  "12s",
                                  "12s_e",
                                  "asv",
                                  "matrix",
                                  "clean_data",
                                  "data12Se_asvmatrix_nc_lor_nfc.csv"),
                       head=TRUE)

ASVtax12su <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "eDNA",
                                  "12s",
                                  "12s_u",
                                  "ASV_taxonomy_12suDNA.csv"),
                       head=TRUE)

sampleASV12su <- read.csv(here::here("Processed_data", #should be ASV by sample
                                     "eDNA",
                                     "12s",
                                     "12s_u",
                                     "asv",
                                     "matrix",
                                     "clean_data",
                                     "data12Su_asvmatrix_nc_lor_nfc.csv"),
                          head=TRUE)


#Formatting data ####

#Select samples within 50m 
#find out which set numbers have greater than 50m between eDNA sample and tral 
trawl <- select(trawl_meta, c("set_number", "depth_mean"))
eDNA <- select(eDNA_meta_12se, c("set_number", "depth"))
depths <- merge(trawl, eDNA, by="set_number")

#figure out all set_numbers that have more than 50m differences 
#make new column which is difference between two values 
depths$difference <- abs(depths$depth_mean - depths$depth)

valid_sets <- subset(depths, difference <= 50, #select only difference less than or equal to 50 
                     select=c(set_number, difference))

valid_set_numbers <- unique(valid_sets$set_number)

valid_set_numbers

#Formatting eDNA data 
#extract sets only in these set numbers 
eDNA_meta_12se <- eDNA_meta_12se[  eDNA_meta_12se$set_number %in% valid_set_numbers, ] 
eDNA_meta_12su <- eDNA_meta_12su[  eDNA_meta_12su$set_number %in% valid_set_numbers, ] 

valid_samples12se <-  unique(eDNA_meta_12se$original_sample_name)
valid_samples12su <-  unique(eDNA_meta_12se$original_sample_name)

samples_12se <- sampleASV12se[  sampleASV12se$original_sample_name %in% valid_samples12se, ] 
samples_12su <- sampleASV12su[  sampleASV12su$original_sample_name %in% valid_samples12su, ] 

#Create Taxa by Site matrix 

ASVbysite <- samples_12se
common_taxa <-  read.csv(here::here("Processed_data", #should be ASV by sample
                                    "eDNA",
                                    "12s",
                                    "12s_e",
                                    "taxonomy_groups_12s_eDNA.csv"),
                         head=TRUE)
taxa <-  read.csv(here::here("Processed_data", #should be ASV by sample
                             "eDNA",
                             "12s",
                             "12s_e",
                             "ASV_taxonomy_12seDNA.csv"),
                  head=TRUE)
  
####

t1 <- as.data.frame(t(ASVbysite))

t2 <- rownames_to_column(t1, var = "ASV") %>% 
  merge(taxa, ., by = "ASV", all.y = T) %>% # lose 5 ASVs that don't have assignment
  merge(common_taxa[c("LCT", "level")],., by = "LCT", all.y = T)

t2 <- rownames_to_column(t1, var = "ASV") %>% 
  merge(taxa, ., by = "ASV", all.y = T) %>% # lose 5 ASVs that don't have assignment
  drop_na(class) %>% #some things made it through occupancy models but were not assigned any taxonomy
  merge(common_taxa[c("LCT", "level")],., by = "LCT", all.y = T)


g1 <- t2 %>%
  filter(class == "Actinopterygii" | class == "Holocephali") %>%
  filter(!is.na(genus)) 

#summarize for each lca_taxon (remove duplication)
t3 <-  t2 %>% 
  group_by(LCT)

s1 <- t3 %>%
  column_to_rownames("LCT") %>%
  t(.) %>%
  as.data.frame()

saveRDS(s1, "Data/2021_09_20/derived_data/lcabysitesurvey_matrix_alltaxa.rds")


#extract samples from occupancy model 


#Need to make ASV by site 

#keep unique ID, north_south, ASV counts 

eDNA12se <- select(eDNA_meta_12se, 21:784)


#need data table with trawl species for each set number 


#merge trawl metadata + eDNA meta by set number 


#Create euler plot 

#Euler plot for gamma diversity between North and South 
#Need: 
  #list of species detected by eDNA in North, list of species detected by eDNA south
  #list of species detect by trawl in South, list of species detected by trawl in North 
#Analysis: 
#Calculate nestedness 


#Old code ####
species <- read_csv(here("Processed_data",
                        "euler",
                        "species_across_methods.csv"))

sp_12se <- read_csv(here("Processed_data",
                         "euler",
                         "all12se.csv"))

sp_12su <- read_csv(here("Processed_data",
                            "euler",
                            "all12su.csv"))

trawl <- read_csv(here("Processed_data",
                       "euler",
                       "alltrawl.csv"))


#using eulerr package 
fit <- euler(c("A" = sums_value[1] , "B" = sums_value[2] , "C" = sums_value[6] , 
               "A&B" = sums_value[3], "B&C" = sums_value[4], 
               "A&B&C" = sums_value[5]),
             shape = "ellipse")

plot(fit)

#using ggvenn package 

df <- list(`Trawl` = c(trawl$genus_species_accepted),
          `12su` = c(sp_12su$genus_species_accepted),
          `12se` = c(sp_12se$genus_species_accepted))

ggvenn(df, c("Trawl", "12su", "12se"))
ggvenn(df,c("Trawl", "12su", "12se"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#I need to make a df that has all species detected by 12se, all species detected by 12su, all species detected by trawl 
#with homogenized names 


           