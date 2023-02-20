---
title: "eDNA Traits Collection"
author: "Anya Mueller"
date: "07/07/2020"
output: html_document
---

#{r setup}
#instal if necessary 
#install.packages("rfishbase",repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"),type="source") 
#install.packages("tidyverse")
#install.packages("taxize")
#install.packages("here")

#load packages
library(rfishbase)
library(tidyverse)
library(taxize)
library(here)


#import the species list data
co1_taxa.orig <- read.delim(here("Data","taxonomy_table.CO1.filtered_collapsed.txt"))
head(co1_taxa.orig)
#check the type for the variables - we want the names to be a character vector
glimpse(co1_taxa.orig)
```

```{r import data}
#import the species list data
co1_taxa.orig <- read.delim(here::here("Data","20200903_taxonomy_table.CO1.merged.collapsed.w_ASV_names.txt"))
head(co1_taxa.orig)
#check the type for the variables - we want the names to be a character vector
glimpse(co1_taxa.orig)
```

```{r data cleaning}
#clean up and select data of interest
co1_taxa <- co1_taxa.orig %>% 
  dplyr::rename_with(~(gsub("X.", "", .))) %>% #fix up column names 
  select("kingdom","phylum", "class","order","family","genus","species") %>% #select columns of interest
  distinct() #pull only distinct rows

#need to make sure that the names of the fish are correct so that they will pull up information when queried against the database
#validate_names allows us to run the list against the fishbase and sealifebase database to see if our names match any entries
#a list is produced that has NAs where the fish names don't match those in the database
#this only works for specie names
#make one object for each database
co1_fb <- validate_names(co1_taxa$species, server = "fishbase") 
co1_fb
co1_sl <- validate_names(co1_taxa$species, server = "sealifebase")
co1_sl
#count up NAs and see how much is accounted for
sum(is.na(co1_fb)) #has 1333 NAs/1346 - has 13 names
sum(is.na(co1_sl)) #has 1034 NAs/1346 - has 312 names
#1021 names are not recognized by sealifebase or fishbase

#lets change these into dataframes and combine them so that we can see what names are not accounted for
f_df <- as.data.frame(co1_fb)
f_df <- f_df %>% drop_na() %>% dplyr::rename("Species" = "co1_fb") %>% mutate("server" = "fishbase")
f_df
s_df <- as.data.frame(co1_sl)
s_df <- s_df %>% drop_na() %>% dplyr::rename("Species" = "co1_sl") %>% mutate("server" = "sealifebase")
s_df
fs_names <- rbind(f_df,s_df)

#lets make a new column in the original data with the fixed names
co1_taxa$Species_fixed
plyr::mapvalues(co1_taxa$species,not_weird,not_in)

co1_taxa$species[fs_names$Species]
```

```{r data cleaning #Delete}
#make a names list with the blanks removed from Rank 8
co1_no.blank <- co1_taxa %>% filter(Rank8 != "") 

#need to make sure that the names of the fish are correct so that they will pull up information when queried against the database
#validate_names allows us to run the list against the database to see if our names match any entries
#a list is produced that has NAs where the fish names don't match those in the database
#we are looking at rank8 because that has the scientific names of the species
co1_fb <- validate_names(co1_no.blank$Rank8, server = "fishbase")
co1_fb
co1_sl <- validate_names(co1_no.blank$Rank8, server = "sealifebase")
co1_sl
#count up NAs and see how much is accounted for
sum(is.na(co1_fb)) #has 152 NAs/156 - has 4 names
sum(is.na(co1_sl)) #has 41 NAs/156 - has 115 names
#we are missing 37 names

#lets change these into dataframes and combine them so that we can see what names are not accounted for
f_df <- as.data.frame(co1_fb)
f_df <- f_df %>% drop_na() %>% dplyr::rename("Species" = "co1_fb") %>% mutate("server" = "fishbase")
f_df
s_df <- as.data.frame(co1_sl)
s_df <- s_df %>% drop_na() %>% dplyr::rename("Species" = "co1_sl") %>% mutate("server" = "sealifebase")
s_df
fs_names <- rbind(f_df,s_df)


#now we can compare co1_names to co1_no_na_unique the fixnames and figure out where the two don't line up, thus telling us which names in our original list have errors in them, ! signifies not in
#this produces a list of the names that weren't queried in fishbase
not_in <- co1_no.blank$Rank8[!co1_no.blank$Rank8%in%fs_names$Species]
not_in
#see some repeats so just pull the unique names
not_in <- unique(not_in) #weird bc this is just 34 names so it seems some are missing or there is some overlap

#the length of the lists seem kinda weird....
fs_names$Species #this is 119 in length
unique(fs_names$Species) #this is 88 in length
unique(co1_no.blank$Rank8) #this is  120 in length
unique(co1_taxa$Rank8) #this is 121 in length 
co1_taxa$Rank8[!co1_taxa$Rank8%in%co1_no.blank$Rank8]

#remove the weird names
not_weird <- str_subset(not_in, pattern = "sp.", negate= TRUE)
not_weird <- str_subset(not_weird, pattern = "cf.", negate= TRUE)
not_weird <- str_subset(not_weird, pattern = "CMC02", negate= TRUE)

not_in_good <- str_replace_all(not_weird, "_", " ")
#lets fix these names in the original file
co1_taxa$Rank8_fixed<-plyr::mapvalues(co1_taxa$Rank8,not_weird,not_in_good)

#tol_resolved() brings up names based on what you queried, they tell you if they are approximate matches or synonyms
#it gives you just one line per name
#this is resolving names using the Open Tree of Life resolver
#n.i.g_resolved <-tol_resolve(not_in_good)
#n.i.g_resolved
#this is the column needed for the replace function bellow
#n.i.g_resolved_names<-as.character(n.i.g_resolved$unique_name)
#lets fix these names in the original file
#co1_taxa$Rank8_fixed2<-plyr::mapvalues(co1_taxa$Rank8,not_in_good,n.i.g_resolved_names) #this didn't do anything, and it doesn't have the corrected names that Rank8_fixed has
#lets check if there are any differences between the two fixed columns
#co1_taxa$Rank8_fixed[!co1_taxa$Rank8_fixed %in% co1_taxa$Rank8_fixed2] 

#check if the fixed names are queriable in rfishbase
n.i.g_resolved_names.fb<-validate_names(not_in_good, server = "fishbase") 
n.i.g_resolved_names.fb #got two more names
n.i.g_resolved_names.sl<-validate_names(not_in_good, server = "sealifebase") 
n.i.g_resolved_names.sl#got two more names
not_in_good
n.i.g_resolved_names.sl
n.i.g_resolved_names.fb 
#lets correct these in co1.taxa
co1_taxa$Rank8_fixed <- str_replace_all(co1_taxa$Rank8_fixed, c("Pleopis polyphemoides"="Pleopis polyphaemoides","Ophiura sarsii"="Ophiura sarsi","Oncorhynchus nerka"="Oncorhynchus nerka","Clupea pallasii"="Clupea pallasii pallasii"))
#join with previous data frame
f_df.r <- as.data.frame(n.i.g_resolved_names.fb)
f_df.r <- f_df.r %>% drop_na() %>% dplyr::rename("Species" = "n.i.g_resolved_names.fb") %>% mutate("server" = "fishbase")
f_df.r
s_df.r <- as.data.frame(n.i.g_resolved_names.sl)
s_df.r <- s_df.r %>% drop_na() %>% dplyr::rename("Species" = "n.i.g_resolved_names.sl") %>% mutate("server" = "sealifebase")
s_df.r
fs_names <- rbind(f_df.r,s_df.r,fs_names)
fs_names <- fs_names %>% distinct()
#make the data frames for the queryable server
fs_names.fb <- fs_names %>% filter(server %in% "fishbase")
fs_names.sl <- fs_names %>% filter(server %in% "sealifebase")
```

```{r all the species level data}
#collecting together all of the species level data for the fish list
#first we group together the ecosystem location data
#ecology() and species() are tables from rfishbase
fb.eco <-ecology(f_df$Species, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic"), server = "fishbase")

fb.sp <-species(f_df$Species,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "fishbase")

fb.lar <- larvae(f_df$Species, field = c("Species","PlaceofDevelopment"))
#merge into one data.frame
fb.info <-list(fb.eco, fb.sp, fb.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "fishbase")
fb.info
#ecology() and species() are tables from sealifebase
sl.eco <-ecology(s_df$Species, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic"), server = "sealifebase")

sl.sp <-species(s_df$Species,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "sealifebase")

sl.lar <- larvae(s_df$Species, field = c("Species","PlaceofDevelopment"))
#merge the ecosystem location data into one data.frame
sl.info <-list(sl.eco, sl.sp, sl.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "sealifebase")
sl.info
#add columns for the binding of the data frames
sl.info <- sl.info %>% mutate("Mesopelagic"=NA, "Bathypelagic"=NA, "Abyssopelagic"=NA, "Hadopelagic"=NA)
#combine the two server's information
info <- rbind(sl.info, fb.info)
#change the binary data into characters so that we can then collapse all theses columns into one
info.wtrpos<-info %>% select("Species","Benthic", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic") %>% mutate(Benthic = ifelse(Benthic==1,"benthic", NA),Demersal = ifelse(Demersal==1,"demersal", NA),Pelagic = ifelse(Pelagic==1,"pelagic", NA),Megabenthos = ifelse(Megabenthos==1,"megabenthos", NA),Macrobenthos = ifelse(Macrobenthos==1,"macrobenthos", NA), Meiobenthos = ifelse(Meiobenthos==1,"meiobenthos", NA),Epipelagic = ifelse(Epipelagic==1,"epipelagic", NA),Mesopelagic = ifelse(Mesopelagic==1,"mesopelagic", NA),Bathypelagic = ifelse(Bathypelagic==1,"bathypelagic", NA),Abyssopelagic = ifelse(Abyssopelagic==1,"abyssopelagic", NA),Hadopelagic = ifelse(Hadopelagic==1,"hadopelagic", NA)) %>% unite(adult_water_position, Benthic:Hadopelagic,sep=", ", na.rm = T) %>% select(Species,adult_water_position)
#change the binary data into characters so that we can then collapse all theses columns into one
info.adltmtl <- info %>% select(Species, Mobile, Sessile) %>%mutate(Mobile = ifelse(Mobile==1,"mobile", NA),Sessile = ifelse(Sessile==1,"sessile", NA)) %>% unite(adult_motility, Mobile, Sessile,sep=", ", na.rm = T) %>% select(Species,adult_motility)
#merge all the dataframes into one
info<-info %>% select("Species","server","DemersPelag", "DepthRangeShallow", "DepthRangeDeep", "PlaceofDevelopment")
info <-list(info, info.adltmtl, info.wtrpos) %>% reduce(left_join, by = "Species")

#add source column and taxonomic resolution, change coloumn name for merging purposes, remove the server column
info <-info %>% mutate(DemersPelag_source = ifelse(is.na(DemersPelag),"",server), DemersPelag_taxonomic_resolution = ifelse(is.na(DemersPelag),"","species"), DepthRangeShallow_source = ifelse(is.na(DepthRangeShallow),"",server), DepthRangeShallow_taxonomic_resolution = ifelse(is.na(DepthRangeShallow),"","species"), DepthRangeDeep_source = ifelse(is.na(DepthRangeDeep),"",server),DepthRangeDeep_taxonomic_resolution = ifelse(is.na(DepthRangeDeep),"","species"),adult_motility_source = ifelse(adult_motility=="","",server),adult_motility_taxonomic_resolution = ifelse(adult_motility=="","","species"),adult_water_position_source = ifelse(adult_water_position=="","",server),adult_water_position_taxonomic_resolution = ifelse(adult_water_position=="","","species"),PlaceofDevelopment_source = ifelse(is.na(PlaceofDevelopment),NA,server),PlaceofDevelopment_taxonomic_resolution = ifelse(is.na(PlaceofDevelopment),NA,"species")) %>% dplyr::rename("Species_fixed"="Species") %>% select(-(server))

#join the trait information to the original data, take away ASV data (which is creating duplicate rows) and pull only unique rows
co1_traits<-left_join(co1_taxa,info, by = "Rank8_fixed") 

#replace the blank spaces with NAs
co1_traits<- as.data.frame(lapply(co1_traits, function(x) { ifelse(x=='', NA, x) }))

#filter so have only one species per row 
co1_traits<-co1_traits %>% select(-(row_names),-(subject)) %>% distinct()
co1_traits
#export csv
write_csv(co1_traits, here("Outputs","co1_traits.csv"))
```

```{r all the species level data #delete}
#collecting together all of the species level data for the fish list
#first we group together the ecosystem location data
#ecology() and species() are tables from rfishbase
fb.eco <-ecology(fs_names.fb$Species, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic"), server = "fishbase")

fb.sp <-species(fs_names.fb$Species,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "fishbase")

fb.lar <- larvae(fs_names.fb$Species, field = c("Species","PlaceofDevelopment"))
#merge into one data.frame
fb.info <-list(fb.eco, fb.sp, fb.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "fishbase")

#ecology() and species() are tables from sealifebase
sl.eco <-ecology(fs_names.sl$Species, field=c("Species","Benthic", "Sessile", "Mobile", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic"), server = "sealifebase")

sl.sp <-species(fs_names.sl$Species,field=c("Species","DemersPelag", "DepthRangeShallow", "DepthRangeDeep"), server = "sealifebase")

sl.lar <- larvae(fs_names.sl$Species, field = c("Species","PlaceofDevelopment"))
#merge the ecosystem location data into one data.frame
sl.info <-list(sl.eco, sl.sp, sl.lar) %>% reduce(left_join, by = "Species") %>% mutate(server = "sealifebase")
fb.info
sl.info
#add columns for the binding of the data frames
sl.info <- sl.info %>% mutate("Mesopelagic"=NA, "Bathypelagic"=NA, "Abyssopelagic"=NA, "Hadopelagic"=NA)
#combine the two server's information
info <- rbind(sl.info, fb.info)
unique(info$PlaceofDevelopment)
#change the binary data into characters so that we can then collapse all theses columns into one
info.wtrpos<-info %>% select("Species","Benthic", "Demersal", "Pelagic","Megabenthos","Meiobenthos","Macrobenthos","Epipelagic", "Mesopelagic", "Bathypelagic", "Abyssopelagic", "Hadopelagic") %>% mutate(Benthic = ifelse(Benthic==1,"benthic", NA),Demersal = ifelse(Demersal==1,"demersal", NA),Pelagic = ifelse(Pelagic==1,"pelagic", NA),Megabenthos = ifelse(Megabenthos==1,"megabenthos", NA),Macrobenthos = ifelse(Macrobenthos==1,"macrobenthos", NA), Meiobenthos = ifelse(Meiobenthos==1,"meiobenthos", NA),Epipelagic = ifelse(Epipelagic==1,"epipelagic", NA),Mesopelagic = ifelse(Mesopelagic==1,"mesopelagic", NA),Bathypelagic = ifelse(Bathypelagic==1,"bathypelagic", NA),Abyssopelagic = ifelse(Abyssopelagic==1,"abyssopelagic", NA),Hadopelagic = ifelse(Hadopelagic==1,"hadopelagic", NA)) %>% unite(adult_water_position, Benthic:Hadopelagic,sep=", ", na.rm = T) %>% select(Species,adult_water_position)
#change the binary data into characters so that we can then collapse all theses columns into one
info.adltmtl <- info %>% select(Species, Mobile, Sessile) %>%mutate(Mobile = ifelse(Mobile==1,"mobile", NA),Sessile = ifelse(Sessile==1,"sessile", NA)) %>% unite(adult_motility, Mobile, Sessile,sep=", ", na.rm = T) %>% select(Species,adult_motility)
#merge all the dataframes into one
info<-info %>% select("Species","server","DemersPelag", "DepthRangeShallow", "DepthRangeDeep", "PlaceofDevelopment")
info <-list(info, info.adltmtl, info.wtrpos) %>% reduce(left_join, by = "Species")

#add source column and taxonomic resolution, change coloumn name for merging purposes, remove the server column
info <-info %>% mutate(DemersPelag_source = ifelse(is.na(DemersPelag),"",server), DemersPelag_taxonomic_resolution = ifelse(is.na(DemersPelag),"","species"), DepthRangeShallow_source = ifelse(is.na(DepthRangeShallow),"",server), DepthRangeShallow_taxonomic_resolution = ifelse(is.na(DepthRangeShallow),"","species"), DepthRangeDeep_source = ifelse(is.na(DepthRangeDeep),"",server),DepthRangeDeep_taxonomic_resolution = ifelse(is.na(DepthRangeDeep),"","species"),adult_motility_source = ifelse(adult_motility=="","",server),adult_motility_taxonomic_resolution = ifelse(adult_motility=="","","species"),adult_water_position_source = ifelse(adult_water_position=="","",server),adult_water_position_taxonomic_resolution = ifelse(adult_water_position=="","","species"),PlaceofDevelopment_source = ifelse(is.na(PlaceofDevelopment),NA,server),PlaceofDevelopment_taxonomic_resolution = ifelse(is.na(PlaceofDevelopment),NA,"species")) %>% dplyr::rename("Rank8_fixed"="Species") %>% select(-(server))

#join the trait information to the original data, take away ASV data (which is creating duplicate rows) and pull only unique rows
co1_traits<-left_join(co1_taxa,info, by = "Rank8_fixed") 

#replace the blank spaces with NAs
co1_traits<- as.data.frame(lapply(co1_traits, function(x) { ifelse(x=='', NA, x) }))

#filter so have only one species per row 
co1_traits<-co1_traits %>% select(-(row_names),-(subject)) %>% distinct()
co1_traits
#export csv
write_csv(co1_traits, here("Outputs","co1_traits.csv"))
```

```{r take a look at what we got}
co1_traits %>%
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

unique(co1_traits$Rank1)
unique(co1_traits$Rank2)
unique(co1_traits$Rank3)
unique(co1_traits$Rank4)
unique(co1_traits$Rank5)
unique(co1_traits$Rank6)
unique(co1_traits$Rank7)
unique(co1_traits$Rank8_fixed)

```

```{r get the body size data}
#pull length data from databases
fb_length<-species(fs_names.fb$Species,field=c("Species","Length","LTypeMaxM"), server = "fishbase") 
fb_length<-fb_length %>% mutate(server= "fishbase")
sl_length<-species(fs_names.sl$Species,field=c("Species","Length","LTypeMaxM"), server = "sealifebase")
sl_length<-sl_length %>% mutate(server = "sealifebase")
#merge
length.data<-rbind(fb_length,sl_length)
length.data

#add source column and taxonomic resolution, change coloumn name for merging purposes, remove the server column
length.data <-length.data %>% mutate(adult_body_size_source = ifelse(is.na(Length),"",server), adult_body_size_taxonomic_resolution = ifelse(is.na(Length),"","species")) %>% dplyr::rename("Rank8_fixed"="Species", "adult_body_size"="Length", "adult_body_size_measurement"= "LTypeMaxM") %>% select(-(server))
length.data

#replace the blank spaces with NAs
length.data<- as.data.frame(lapply(length.data, function(x) { ifelse(x=='', NA, x) }))
length.data
#join the trait information to the current database data 
CO1.db<-read_csv(here::here("Data", "20200921_CO1_traits.csv"))
co1.db.w.lengths<-left_join(CO1.db,length.data, by = "Rank8_fixed") 

#export csv
write_csv(co1.db.w.lengths, here::here("Outputs","co1.db.w.lengths.csv"))
#then will replace the google sheet with this one
```

```{r collecting traits for higher taxonomies}
#filter for classifications only to higher taxonomic classifications
co1_taxa.norank8<-co1_taxa.orig %>% filter(Rank8 == "") %>% select(-row_names, -subject) %>% distinct()
#replace the blank spaces with NAs
co1_taxa.norank8 <-as.data.frame(lapply(co1_taxa.norank8, function(x) { ifelse(x=='', "NA", x) }))
co1_taxa.norank8
#make a vector for each of the ranks which have no ranks at lower taxo levels
rank7.norank8 <- co1_taxa.norank8 %>% filter(Rank7 != "NA") %>% select(Rank7)
rank6.norank7 <- co1_taxa.norank8 %>% filter(Rank7 == "NA", Rank6 != "NA") %>% select(Rank6)
rank5.norank6 <- co1_taxa.norank8 %>% filter(Rank6 == "NA", Rank5 != "NA" ) %>% select(Rank5)
rank4.norank5 <- co1_taxa.norank8 %>% filter(Rank5 == "NA", Rank4 != "NA") %>% select(Rank4)
#these ones are all zero so we don't need them
#rank3.norank4 <- co1_taxa.norank8 %>% filter(Rank4 == "NA", Rank3 != "NA") %>% select(Rank3)
#rank2.norank3 <- co1_taxa.norank8 %>% filter(Rank3 == "NA", Rank2 != "NA") %>% select(Rank2)
#rank1.norank2 <- co1_taxa.norank8 %>% filter(Rank2 == "NA", Rank1 != "NA") %>% select(Rank1)

#pull a table of taxonomic information from sealifebase and fishbase
fb.taxa.list<-as.data.frame(load_taxa())
sl.taxa.list<-as.data.frame(load_taxa(server = "sealifebase"))

####SEALIFE BASE rank7.norank8####
#filter for only genus of rank7.norank8
sl.rank7.norank8 <- sl.taxa.list %>% filter(Genus %in% rank7.norank8$Rank7)

####SEALIFE BASE rank7.norank8####
#then want to feed each species found into the species function to pull up all their information
info.sl.rank7.norank8 <-species(sl.rank7.norank8$Species, server = "sealifebase")
fltrd.info.sl.rank7.norank8 <-info.sl.rank7.norank8 %>% 
  select(Genus, DemersPelag,DepthRangeShallow,DepthRangeDeep,Length, LTypeMaxM) %>% #subset to columns of interest
  distinct() #only take rows that are different from each other


###now we want to reduce the rows to one per each genus or extract the information for each genus 

#first lets calculate all our statistics
clcltd.info.sl.rank7.norank8<- fltrd.info.sl.rank7.norank8 %>% group_by(Genus) %>% #split into groups by genus
  mutate(mean_adult_body_size = mean(as.numeric(Length), na.rm = TRUE), standard_deviation_adult_body_size = sd(as.numeric(Length), na.rm = TRUE), has_adult_body_size = ifelse(is.na(Length),0,1), number_species_adult_body_size = sum(has_adult_body_size)) %>% #we are calculating the statistics for the random variable length - we get mean, standard deviation and n
 mutate(mean_DepthRangeShallow = mean(as.numeric(DepthRangeShallow), na.rm = TRUE), standard_deviation_DepthRangeShallow = sd(as.numeric(DepthRangeShallow), na.rm = TRUE), has_DepthRangeShallow = ifelse(is.na(DepthRangeShallow),0,1), number_species_DepthRangeShallow = sum(has_DepthRangeShallow), min_DepthRangeShallow = min(as.numeric(DepthRangeShallow), na.rm = TRUE), max_DepthRangeShallow = max(as.numeric(DepthRangeShallow), na.rm = TRUE)) %>%  #we are calculating the statistics for the random variable DepthRangeShallow - we get mean, standard deviation,n, min and max #### min and max of empty set is inf
mutate(mean_DepthRangeDeep = mean(as.numeric(DepthRangeDeep), na.rm = TRUE), standard_deviation_DepthRangeDeep = sd(as.numeric(DepthRangeDeep), na.rm = TRUE), has_DepthRangeDeep = ifelse(is.na(DepthRangeDeep),0,1), number_species_DepthRangeDeep = sum(has_DepthRangeDeep), min_DepthRangeDeep = min(as.numeric(DepthRangeDeep), na.rm = TRUE), max_DepthRangeDeep = max(as.numeric(DepthRangeDeep), na.rm = TRUE)) %>% #we are calculating the statistics for the random variable DepthRangeDeep - we get mean, standard deviation,n, min and max #### min and max of empty set is inf
  select(-DepthRangeShallow,-DepthRangeDeep,-Length,-has_DepthRangeShallow,-has_adult_body_size,-has_DepthRangeDeep) %>% #remove rows that we just did calculations with 
  distinct() #so that we can cut down the data frame again

#now lets figure out what to do with the qualitative data
#want: adult_water_position = benthic, pelagic, demersal, biphasic_pelagic_benthic
#adult_motility = sessile, motile, biphasic_motile_sessile

#just working with DemersPelag
demerpelag.info.sl.rank7.norank8<- clcltd.info.sl.rank7.norank8 %>%
  select(Genus,DemersPelag) %>% #select columns of interest
  filter(!is.na(DemersPelag)) %>%#remove NAs
  distinct() %>% #take only distinct rows
  rownames_to_column(var="rowid") %>% #make another column for the pivot
  pivot_wider(names_from=DemersPelag, values_from = rowid, values_fn = length) #pivot table to get one line per genus
#now we need to colapse this all again
#check colnames so that we know if more columns need to be added into the mutate preformed below
colnames(demerpelag.info.sl.rank7.norank8)
awp.info.sl.rank7.norank8<-demerpelag.info.sl.rank7.norank8 %>% dplyr::rename("reef_associated" = "reef-associated") %>% #change name so can use it in mutate function
  mutate(pelagic = ifelse(is.na(pelagic),"NA","pelagic"), 
                                            demersal = ifelse(is.na(demersal),"NA","demersal"), 
                                            epiphytic = ifelse(is.na(epiphytic),"NA","epiphytic"), 
                                            benthic = ifelse(is.na(benthic),"NA","benthic"),
                                            reef_associated = ifelse(is.na(reef_associated),"NA","reef_associated"),
                                            sessile = ifelse(is.na(sessile),"NA","sessile"), 
                                            benthopelagic = ifelse(is.na(benthopelagic),"NA","benthopelagic")) %>% #change the numbers to names in order to combine them into one column
  mutate(adult_water_position= str_remove_all(str_remove_all(paste(pelagic,demersal,epiphytic,benthic,reef_associated,sessile,benthopelagic, sep = "_"),"_NA"),"NA_")) %>% #create new column with the water position information, removing all the NAs
  select(Genus, adult_water_position) #slect the columns of interest

#just working with LTypeMaxM
ltype.info.sl.rank7.norank8<-clcltd.info.sl.rank7.norank8 %>% select(Genus,LTypeMaxM) %>% #select columns of interest
  filter(!is.na(LTypeMaxM)) %>% #remove NAs
  distinct() %>% #get only distinct rows
  rownames_to_column(var="rowid") %>% #make another column for the pivot
  pivot_wider(names_from=LTypeMaxM, values_from = rowid, values_fn = length) #pivot table to get one line per genus

#now we need to colapse it all again
#check colnames so that we know if more columns need to be added into the mutate preformed below
colnames(ltype.info.sl.rank7.norank8)
absm.info.sl.rank7.norank8<-ltype.info.sl.rank7.norank8 %>% mutate(WD = ifelse(is.na(WD), "NA", "WD"),
                                       TL = ifelse(is.na(TL), "NA", "TL"),
                                       DL = ifelse(is.na(DL), "NA", "DL"),
                                       SHH = ifelse(is.na(SHH), "NA", "SHH"),
                                       SHL = ifelse(is.na(SHL), "NA", "SHL"),
                                       SL = ifelse(is.na(SL), "NA", "SL"),
                                       H = ifelse(is.na(H), "NA", "H"),) %>% #change the numbers to names in order to combine them into one column
  mutate(adult_body_size_measurement = str_remove_all(str_remove_all(paste(WD,TL,DL,SHH,SHL,SL,H, sep = "_"),"_NA"),"NA_")) %>% #create new column with the water position information, removing all the NAs
  select(Genus, adult_body_size_measurement) #slect the columns of interest

#now lets combine all the info back together

all.info.sl.rank7.norank8<-list(clcltd.info.sl.rank7.norank8,awp.info.sl.rank7.norank8,absm.info.sl.rank7.norank8) %>% reduce(left_join, by = "Genus") %>% #join the data frames
  select(-DemersPelag,-LTypeMaxM) %>% #remove columns of no interest
  distinct() %>%#take only distinct rows - we want one per genus
  mutate(min_DepthRangeShallow = str_replace_all(min_DepthRangeShallow, "Inf", "NA"),max_DepthRangeShallow = str_replace_all(max_DepthRangeShallow, "-Inf", "NA"),min_DepthRangeDeep = str_replace_all(min_DepthRangeDeep, "Inf", "NA"),max_DepthRangeDeep = str_replace_all(max_DepthRangeDeep, "-Inf", "NA"))  %>% #remove the "Inf" for the min and max columns
  mutate(across(everything(), ~replace_na(.x, "NA"))) %>% #replace NAs across the whole data frame with character NA
  mutate(mean_adult_body_size_source = ifelse(mean_adult_body_size=="NA", "NA","sealifebase"),standard_deviation_adult_body_size_source = ifelse(standard_deviation_adult_body_size == "NA", "NA", "sealifebase"),number_species_adult_body_size_source = ifelse(number_species_adult_body_size == "NA", "NA", "sealifebase"),mean_DepthRangeShallow_source = ifelse(mean_DepthRangeShallow == "NA", "NA", "sealifebase"),standard_deviation_DepthRangeShallow_source = ifelse(standard_deviation_DepthRangeShallow == "NA", "NA", "sealifebase"), number_species_DepthRangeShallow_source = ifelse(number_species_DepthRangeShallow == "NA", "NA", "sealifebase"),min_DepthRangeShallow_source = ifelse(min_DepthRangeShallow == "NA", "NA", "sealifebase"),max_DepthRangeShallow_source = ifelse(max_DepthRangeShallow == "NA", "NA", "sealifebase"),mean_DepthRangeDeep_source = ifelse(mean_DepthRangeDeep == "NA", "NA", "sealifebase"),standard_deviation_DepthRangeDeep_source = ifelse(standard_deviation_DepthRangeDeep == "NA", "NA", "sealifebase"),number_species_DepthRangeDeep_source = ifelse(number_species_DepthRangeDeep == "NA", "NA", "sealifebase"), min_DepthRangeDeep_source = ifelse(min_DepthRangeDeep == "NA", "NA", "sealifebase"), max_DepthRangeDeep_source = ifelse(max_DepthRangeDeep == "NA", "NA", "sealifebase"),adult_water_position_source = ifelse(adult_water_position == "NA", "NA", "sealifebase"),adult_body_size_measurement_source = ifelse(adult_body_size_measurement == "NA", "NA", "sealifebase")) #add source column for each measurement

#export it to show the team:
write_csv(all.info.sl.rank7.norank8, here::here("Outputs", "all.info.sl.rank7.norank8.csv"))

####FISHBASE rank7.norank8 #### THERE AREN'T ANY IN THE CURRENT SPECIES LIST, BUT SHOULD BE ABLE TO RUN THIS IF WE DO HAVE SOME

#filter for only genus of rank7.norank8
fb.rank7.norank8 <- fb.taxa.list %>% filter(Genus %in% rank7.norank8$Rank7)

#we want to feed each species found into the species function to pull up all their information
info.fb.rank7.norank8 <-species(fb.rank7.norank8$Species, server = "sealifebase")
fltrd.info.fb.rank7.norank8 <-info.fb.rank7.norank8 %>% 
  select(Genus, DemersPelag,DepthRangeShallow,DepthRangeDeep,Length, LTypeMaxM) %>% #subset to columns of interest
  distinct() #only take rows that are different from each other


###now we want to reduce the rows to one per each genus or extract the information for each genus 

#first lets calculate all our statistics
clcltd.info.fb.rank7.norank8<- fltrd.info.fb.rank7.norank8 %>% group_by(Genus) %>% #split into groups by genus
  mutate(mean_adult_body_size = mean(as.numeric(Length), na.rm = TRUE), standard_deviation_adult_body_size = sd(as.numeric(Length), na.rm = TRUE), has_adult_body_size = ifelse(is.na(Length),0,1), number_species_adult_body_size = sum(has_adult_body_size)) %>% #we are calculating the statistics for the random variable length - we get mean, standard deviation and n
 mutate(mean_DepthRangeShallow = mean(as.numeric(DepthRangeShallow), na.rm = TRUE), standard_deviation_DepthRangeShallow = sd(as.numeric(DepthRangeShallow), na.rm = TRUE), has_DepthRangeShallow = ifelse(is.na(DepthRangeShallow),0,1), number_species_DepthRangeShallow = sum(has_DepthRangeShallow), min_DepthRangeShallow = min(as.numeric(DepthRangeShallow), na.rm = TRUE), max_DepthRangeShallow = max(as.numeric(DepthRangeShallow), na.rm = TRUE)) %>%  #we are calculating the statistics for the random variable DepthRangeShallow - we get mean, standard deviation,n, min and max #### min and max of empty set is inf
mutate(mean_DepthRangeDeep = mean(as.numeric(DepthRangeDeep), na.rm = TRUE), standard_deviation_DepthRangeDeep = sd(as.numeric(DepthRangeDeep), na.rm = TRUE), has_DepthRangeDeep = ifelse(is.na(DepthRangeDeep),0,1), number_species_DepthRangeDeep = sum(has_DepthRangeDeep), min_DepthRangeDeep = min(as.numeric(DepthRangeDeep), na.rm = TRUE), max_DepthRangeDeep = max(as.numeric(DepthRangeDeep), na.rm = TRUE)) %>% #we are calculating the statistics for the random variable DepthRangeDeep - we get mean, standard deviation,n, min and max #### min and max of empty set is inf
  select(-DepthRangeShallow,-DepthRangeDeep,-Length,-has_DepthRangeShallow,-has_adult_body_size,-has_DepthRangeDeep) %>% #remove rows that we just did calculations with 
  distinct() #so that we can cut down the data frame again

#now lets figure out what to do with the qualitative data
#want: adult_water_position = benthic, pelagic, demersal, biphasic_pelagic_benthic
#adult_motility = sessile, motile, biphasic_motile_sessile

#just working with DemersPelag
demerpelag.info.fb.rank7.norank8<- clcltd.info.fb.rank7.norank8 %>%
  select(Genus,DemersPelag) %>% #select columns of interest
  filter(!is.na(DemersPelag)) %>%#remove NAs
  distinct() %>% #take only distinct rows
  rownames_to_column(var="rowid") %>% #make another column for the pivot
  pivot_wider(names_from=DemersPelag, values_from = rowid, values_fn = length) #pivot table to get one line per genus
#now we need to colapse this all again
#check colnames so that we know if more columns need to be added into the mutate preformed below
colnames(demerpelag.info.fb.rank7.norank8)
awp.info.fb.rank7.norank8<-demerpelag.info.fb.rank7.norank8 %>% dplyr::rename("reef_associated" = "reef-associated") %>% #change name so can use it in mutate function
  mutate(pelagic = ifelse(is.na(pelagic),"NA","pelagic"), 
                                            demersal = ifelse(is.na(demersal),"NA","demersal"), 
                                            epiphytic = ifelse(is.na(epiphytic),"NA","epiphytic"), 
                                            benthic = ifelse(is.na(benthic),"NA","benthic"),
                                            reef_associated = ifelse(is.na(reef_associated),"NA","reef_associated"),
                                            sessile = ifelse(is.na(sessile),"NA","sessile"), 
                                            benthopelagic = ifelse(is.na(benthopelagic),"NA","benthopelagic")) %>% #change the numbers to names in order to combine them into one column
  mutate(adult_water_position= str_remove_all(str_remove_all(paste(pelagic,demersal,epiphytic,benthic,reef_associated,sessile,benthopelagic, sep = "_"),"_NA"),"NA_")) %>% #create new column with the water position information, removing all the NAs
  select(Genus, adult_water_position) #slect the columns of interest

#just working with LTypeMaxM
ltype.info.fb.rank7.norank8<-clcltd.info.fb.rank7.norank8 %>% select(Genus,LTypeMaxM) %>% #select columns of interest
  filter(!is.na(LTypeMaxM)) %>% #remove NAs
  distinct() %>% #get only distinct rows
  rownames_to_column(var="rowid") %>% #make another column for the pivot
  pivot_wider(names_from=LTypeMaxM, values_from = rowid, values_fn = length) #pivot table to get one line per genus

#now we need to colapse it all again
#check colnames so that we know if more columns need to be added into the mutate preformed below
colnames(ltype.info.fb.rank7.norank8)
absm.info.fb.rank7.norank8<-ltype.info.fb.rank7.norank8 %>% mutate(WD = ifelse(is.na(WD), "NA", "WD"),
                                       TL = ifelse(is.na(TL), "NA", "TL"),
                                       DL = ifelse(is.na(DL), "NA", "DL"),
                                       SHH = ifelse(is.na(SHH), "NA", "SHH"),
                                       SHL = ifelse(is.na(SHL), "NA", "SHL"),
                                       SL = ifelse(is.na(SL), "NA", "SL"),
                                       H = ifelse(is.na(H), "NA", "H"),) %>% #change the numbers to names in order to combine them into one column
  mutate(adult_body_size_measurement = str_remove_all(str_remove_all(paste(WD,TL,DL,SHH,SHL,SL,H, sep = "_"),"_NA"),"NA_")) %>% #create new column with the water position information, removing all the NAs
  select(Genus, adult_body_size_measurement) #slect the columns of interest

#now lets combine all the info back together

all.info.fb.rank7.norank8<-list(clcltd.info.fb.rank7.norank8,awp.info.fb.rank7.norank8,absm.info.fb.rank7.norank8) %>% reduce(left_join, by = "Genus") %>% #join the data frames
  select(-DemersPelag,-LTypeMaxM) %>% #remove columns of no interest
  distinct() %>%#take only distinct rows - we want one per genus
  mutate(min_DepthRangeShallow = str_replace_all(min_DepthRangeShallow, "Inf", "NA"),max_DepthRangeShallow = str_replace_all(max_DepthRangeShallow, "-Inf", "NA"),min_DepthRangeDeep = str_replace_all(min_DepthRangeDeep, "Inf", "NA"),max_DepthRangeDeep = str_replace_all(max_DepthRangeDeep, "-Inf", "NA"))  %>% #remove the "Inf" for the min and max columns
  mutate(across(everything(), ~replace_na(.x, "NA"))) %>% #replace NAs across the whole data frame with character NA
  mutate(mean_adult_body_size_source = ifelse(mean_adult_body_size=="NA", "NA","sealifebase"),standard_deviation_adult_body_size_source = ifelse(standard_deviation_adult_body_size == "NA", "NA", "sealifebase"),number_species_adult_body_size_source = ifelse(number_species_adult_body_size == "NA", "NA", "sealifebase"),mean_DepthRangeShallow_source = ifelse(mean_DepthRangeShallow == "NA", "NA", "sealifebase"),standard_deviation_DepthRangeShallow_source = ifelse(standard_deviation_DepthRangeShallow == "NA", "NA", "sealifebase"), number_species_DepthRangeShallow_source = ifelse(number_species_DepthRangeShallow == "NA", "NA", "sealifebase"),min_DepthRangeShallow_source = ifelse(min_DepthRangeShallow == "NA", "NA", "sealifebase"),max_DepthRangeShallow_source = ifelse(max_DepthRangeShallow == "NA", "NA", "sealifebase"),mean_DepthRangeDeep_source = ifelse(mean_DepthRangeDeep == "NA", "NA", "sealifebase"),standard_deviation_DepthRangeDeep_source = ifelse(standard_deviation_DepthRangeDeep == "NA", "NA", "sealifebase"),number_species_DepthRangeDeep_source = ifelse(number_species_DepthRangeDeep == "NA", "NA", "sealifebase"), min_DepthRangeDeep_source = ifelse(min_DepthRangeDeep == "NA", "NA", "sealifebase"), max_DepthRangeDeep_source = ifelse(max_DepthRangeDeep == "NA", "NA", "sealifebase"),adult_water_position_source = ifelse(adult_water_position == "NA", "NA", "sealifebase"),adult_body_size_measurement_source = ifelse(adult_body_size_measurement == "NA", "NA", "sealifebase")) #add source column for each measurement


#repeat this process with higher taxo ranks

```

