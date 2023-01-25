#Goal: make initial Euler plots 

#Set-up 
library(dplyr)
library(eulerr)
library(tidyr)
library(tidyverse)

trawl_spp <- read_csv(here("processed_data",
                                 "trawl",
                                 "clean_data",
                                 "trawl_taxonomy.csv"))

spp12se <- read_csv(here("processed_data",
                           "eDNA",
                           "12se",
                            "clean_data",
                           "taxonomy",
                           "data12se_taxonomy_r_nc_lor_nfc_a_nt.csv"))

spp12su <- read_csv(here("processed_data",
                         "eDNA",
                         "12su",
                         "clean_data",
                         "taxonomy",
                         "data12su_taxonomy_r_nc_lor_nfc_a_nt.csv"))

nordic <- read_csv(here("rawdata",
                         "trawl",
                         "trawl_raw_species.csv"))

#remove whitespace

trimws(trawl_spp$species, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

#Species level Euler plots 

#manipulating trawl data 
#add column to trawl indicating caught_type 
trawl_spp$caught_type <- c('trawl')

#remove column "common_name" and "synonym" 
trawl_spp <- trawl_spp %>% select(-common_name)
trawl_spp <- trawl_spp %>% select(-synonym)

#do species taxonomy number match raw data? 
unique(nordic$Species)
unique(trawl_spp$species)
#yes-ish

#manipulating eDNA data
#add column to eDNA 12s indicating caught type 
spp12se$caught_type <- c('12semeta')
spp12su$caught_type <- c('12sumeta')

#merge all eDNA data together 
metaspp <- rbind(spp12se, spp12su)

#remove column "query" 
metaspp <- metaspp %>% select(-query)

#View(metaspp)

#merge all data together 
alldata <- bind_rows(metaspp, trawl_spp) 

#work needs to be done in merging the species names between eDNA and trawl, 
#some of the names aren't getting considered to be both
#kind of want to use species ID instead 

#subset for 'Chordata' only 
alldata <- alldata[alldata$phylum %in% c('Chordata'),]

#View(alldata)

#subset for species level only 
#remove NA values 
c(alldata$species)
alldata<- alldata[!is.na(alldata$species),]
alldata <- alldata %>% drop_na(species)

#View(alldata)

#get rid of duplicated rows 
alldata <- alldata %>% distinct(species, caught_type, .keep_all = TRUE)

#View(alldata)

#summarize data
#make A,B,C, AB, BC, ABC parameters 
alldata <- alldata %>% 
  group_by(species, caught_type) %>%    
  mutate(Group =
        case_when(caught_type == 'trawl' ~ "A", 
        caught_type == '12semeta' ~ "B",
        caught_type == '12sumeta' ~ "C")
)


alldata <- alldata %>% 
  group_by(species) %>% 
  mutate(newcol = paste(Group, collapse= ' '))

#View(alldata)

#sum A, B, C, AB, BC
sums <- table(alldata$newcol)
sums_value <- as.numeric(sums)

#A equals the subset of trawl 
#B equals the subset of 12se
#C equals the subset of 12su 
  
#make plot

#input these numbers into the plot

fit <- euler(c("A" = sums_value[1] , "B" = sums_value[2] , "C" = sums_value[6] , 
               "A&B" = sums_value[3], "B&C" = sums_value[4], 
               "A&B&C" = sums_value[5]),
             shape = "ellipse")

plot(fit)

#input these numbers into the plot

#Gamma Diversity Euler Plot 
#Author: Tessa Rehill 

#Set-up 
#install.packages('ggvenn')
#install.packages('RColorBrewer')

library(dplyr)
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)

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

