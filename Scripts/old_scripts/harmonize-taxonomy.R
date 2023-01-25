##Taxonomic harmonization between trawl and 12se and 12su
#Set-Up 
#Author: Nikki Moore
#Edited by: Tessa Rehill 

#Set-Up
install.packages("taxadb")

library(plyr)
library(tidyverse)
library(tidyr)
library(taxadb)
library(here)


## read in data files 
## read in trawl species 
trawl_clean <- read.csv(here::here("Processed_data",
                             "trawl",
                             "catch_data",
                             "clean_data",
                             "trawl_taxonomy_clean_species.csv"),
                  head=TRUE)

colnames(trawl_clean) <- c('species')

## read in 12se species 
spp_12se <- read.csv(here::here("Processed_data",
                                "eDNA",
                                "12s",
                                "12s_e",
                                "taxonomy",
                                "clean_data",
                                "data12se_taxonomy_r_nc_lor_nfc_a_nt.csv"),
                     head=TRUE)


spp_12su <- read.csv(here::here("Processed_data",
                                "eDNA",
                                "12s",
                                "12s_u",
                                "taxonomy",
                                "clean_data",
                                "data12su_taxonomy_r_nc_lor_nfc_a_nt.csv"),
                     head=TRUE)


##trawl clean data 
trawl_clean <- read.csv(here::here("Processed_data",
                             "trawl",
                             "catch_data",
                             "clean_data",
                             "trawl_taxonomy_clean_species.csv"),
                  head=TRUE)

## function to harmonize a list of species names 
harmonize_taxnomy <- function(names) {
  
  ## first, search for GBIF accepted names
  ## GBIF
  gbif = filter_name(names, "gbif") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "gbif") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus)
  
  not_found <- names[which(!names %in% gbif$input)]
  
  ## if not found, search itis
  ## itis
  itis = filter_name(not_found, "itis") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "itis") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus)
  
  not_found <- not_found[which(!not_found %in% itis$input)]
  
  ## if not found, search col
  ## col
  col = filter_name(not_found, "col") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "col") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus)
  
  not_found <- not_found[which(!not_found %in% col$input)]
  
  ## if not found, search ncbi
  ## ncbi
  animal_classes <- c("Aves", "Mammalia", "Reptilia", "Insecta",  "Chondrichthyes", "Amphibia",
                      "Actinopterygii","Teleostei", "Bivalvia", "Elasmobranchii", "Holocephali")
  
  ncbi = filter_name(not_found, "ncbi") %>%
    filter(taxonomicStatus == "accepted") %>%
    mutate(db = "ncbi") %>%
    rename("db_code" = acceptedNameUsageID) %>%
    select(input, scientificName, db, db_code, kingdom, order, class, family, genus) %>%
    mutate(kingdom = ifelse(kingdom == "Metazoa" & class %in% animal_classes,
                            "Animalia",
                            "Plantae")) ## change kingdom = metazoa to plantae or animalia
  
  not_found <- not_found[which(!not_found %in% ncbi$input)]
  
  names_db <- rbind(itis, ncbi) %>% rbind(., col) %>% rbind(., gbif) %>%
    unique(.) %>%
    rename("genus_species" = input,
           "genus_species_accepted" = scientificName) 
  
  ## return database of names and list of species not found
  
  return(list(names_db, not_found))
}

##compare trawl to 12se ####
## harmonize taxonomy 
harmonized_acctrawl <- harmonize_taxnomy(trawl_clean$species)

acc_accsptrawl <- harmonized_acctrawl[[1]]
acc_notfsptrawl <- harmonized_acctrawl[[2]]

## find out which overlap
length(which(acc_accsptrawl$genus_species_accepted %in% spp_12se$species)) #12
length(which(acc_notfsptrawl %in% spp_12se$species)) #0

## generate list of the overlapping species
sp_overlaptrawl_12se <- acc_accsptrawl$genus_species_accepted[which(acc_accsptrawl$genus_species_accepted %in% spp_12se$species)]

#create dataframe of overlapping species 
sp_trawl_12se <- data.frame(sp_overlaptrawl_12se) 

#change column names so you merge all later 
colnames(sp_trawl_12se) <- c('sp_trawl_12se')

#check that this df makes sense 
unique(spp_12se$species) #29 
unique(trawl_clean$species) #25 #need to remove NA from clean species list 
unique(sp_trawl_12se$sp_trawl_12se) 

write_csv(sp_trawl_12se,
          here("Processed_data",
               "euler",
               "trawl_12se_species.csv"))


##compare trawl to 12su ####
## harmonize taxonomy 
harmonized_acc <- harmonize_taxnomy(spp_12su$species)

acc_accsp12su <- harmonized_acc[[1]]
acc_notfsp12su <- harmonized_acc[[2]]

## find out which overlap
length(which(acc_accsptrawl$genus_species_accepted %in% spp_12su$species)) #10
length(which(acc_notfsptrawl %in% spp_12su$species)) #0

## generate list of the overlapping species
sp_overlaptrawl_12su <- acc_accsptrawl$genus_species_accepted[which(acc_accsptrawl$genus_species_accepted %in% spp_12su$species)]

#create dataframe of overlapping species 
sp_trawl_12su <- data.frame(sp_overlaptrawl_12su) 

#change column names so you merge all later 
colnames(sp_trawl_12su) <- c('sp_trawl_12su')

#check that this df makes sense 
unique(spp_12su$species) #22 
unique(trawl_clean$species) #25 #need to remove NA from clean species list 
unique(sp_trawl_12su$sp_trawl_12su) #10

write_csv(sp_trawl_12su,
          here("Processed_data",
               "euler",
               "trawl_12su_species.csv"))

##compare 12se to 12su ####
## harmonize taxonomy 
harmonized_acc <- harmonize_taxnomy(spp_12se$species)

acc_accsp12se <- harmonized_acc[[1]]
acc_notfsp12se <- harmonized_acc[[2]]

## find out which overlap
length(which(acc_accsp12se$genus_species_accepted %in% spp_12su$species)) #18
length(which(acc_notfsp12se %in% spp_12su$species)) #0

## generate list of the overlapping species
sp_overlap12se12su <- acc_accsp12se$genus_species_accepted[which(acc_accsp12se$genus_species_accepted %in% spp_12su$species)]

#create dataframe of overlapping species 
sp_12se12su <- data.frame(sp_overlap12se12su) 

#change column names so you merge all later 
colnames(sp_12se12su) <- c('species12se12su')

#check that this df makes sense 
unique(spp_12se$species) #29 
unique(spp_12su$species) #21
unique(sp_12se12su$species12se12su) #18

write_csv(sp_12se12su,
          here("Processed_data",
               "euler",
               "12se_12su_species.csv"))

#Species found in all methods ####
#acc_accsptrawl - trawl species list 
#acc_accsp12se - 12se species list 
#acc_accsp12su - 12su species list 

#these are NOT unique, but full lists that could be found in other methods 
#created Venn diagram that can help with this visualization

a <- acc_accsp12se
b <- acc_accsp12su
c <- acc_accsptrawl 

#sp_overlaptrawl_12se -trawl 12se species list
#sp_overlaptrawl_12su - trawl 12su species list 
#sp_overlap12se12su  - 12se and 12su species list 

d <- as.data.frame(sp_overlap12se12su)
e <- as.data.frame(sp_overlaptrawl_12su)
f <- as.data.frame(sp_overlaptrawl_12se)

#all. find species found in ALL methods 
#this means they will be in d, e, f 

all <- intersect(d,e)
all <- intersect(all, f)


all <- as.data.frame(all)
colnames(all) <- c('all')

write_csv(all,
          here("Processed_data",
               "euler",
               "trawl_eDNA_species"))


#a. isolate 12se species only ####
#these species are in 'a' but not in d,f, and all 

#what is in 12se(a) but not in 12se/12su(d)
s_12se <- setdiff(a$genus_species_accepted,d$sp_overlap12se12su) 
#what is in above not in trawl(f)
s_12se <- setdiff(s_12se,f) 
#what is in above but not in all 
s_12se <- setdiff(s_12se,all) 

#create dataframe of unique species
s_12se<- data.frame(s_12se) 

#change column names so you merge all later 
colnames(s_12se) <- c('only12se')

write_csv(s_12se,
          here("Processed_data",
               "euler",
               "speciesonly12se"))

#b. isolate 12su species only ####
#these species are in 'b' but not in d, e, or all 

#what is in 12se(u) but not in 12su/trawl(e)
s_12su <- setdiff(b$genus_species_accepted,e$sp_overlaptrawl_12su) 
#what is in above not in 12se(d)
s_12su <- setdiff(s_12su,d) 
#what is in above but not in all 
s_12su <- setdiff(s_12su,all) 

#create dataframe of unique species
s_12su<- data.frame(s_12su) 

#change column names so you merge all later 
colnames(s_12su) <- c('only12su')

write_csv(s_12se,
          here("Processed_data",
               "euler",
               "speciesonly12su"))


#c. isolate trawl species only ####
#what is in trawl(c) but not in f, e or all 

#what is in trawl(c) but not in 12se/trawl(f)
s_trawl <- setdiff(c$genus_species_accepted,f$sp_overlaptrawl_12se) 
#what is in above not in trawl(e)
s_trawl<- setdiff(s_trawl,e) 
#what is in above but not in all 
s_trawl <- setdiff(s_trawl,all) 

#create dataframe of unique species
s_trawl<- data.frame(s_trawl) 

#change column names so you merge all later 
colnames(s_trawl) <- c('onlytrawl')

write_csv(s_trawl,
          here("Processed_data",
               "euler",
               "speciesonlytrawl"))

#f. find 12se/trawl(f) not in all 
unique12setrawl<- setdiff(f$sp_overlaptrawl_12se, all$all)
unique12setrawl<- as.data.frame(unique12setrawl) 
 
 #change column names so you merge all later 
 colnames(unique12setrawl) <- c('12se_trawl_only')
 
 write_csv(unique12setrawl,
           here("Processed_data",
                "euler",
                "unique12setrawl"))
 
 
 #d. find 12se/12su(d) not in all 
 
unique12su12se <- setdiff(d$sp_overlap12se12su, all$all)
unique12su12se <- as.data.frame(unique12su12se)

colnames(unique12su12se)<-c('12se_12su_only')

write_csv(unique12su12se,
          here("Processed_data",
               "euler",
               "unique12se12su"))

#e. find 12su/trawl(e) not in all 

unique12sutrawl <- setdiff(e$sp_overlaptrawl_12su, all$all) #none 
unique12sutrawl <- as.data.frame(unique12sutrawl)

colnames(unique12sutrawl)<-c('12se_trawl_only')

write_csv(unique12sutrawl,
          here("Processed_data",
               "euler",
               "unique12sutrawl"))

#merge all lists together ###

new <- rbind.fill(s_trawl, s_12se, s_12su, all, unique12setrawl, unique12su12se, unique12sutrawl)

write_csv(new,
          here("Processed_data",
               "euler",
               "species_across_methods.csv"))

#add ASVs

#I need to make a df that has all species detected by 12se, all species detected by 12su, all species detected by trawl 
#with homogenized names 
a <- acc_accsp12se
b <- acc_accsp12su
c <- acc_accsptrawl
#remove NA values in trawl 
c<- subset(c, !is.na(genus_species_accepted))


write_csv(a,
          here("Processed_data",
               "euler",
               "all12se.csv"))

write_csv(b,
          here("Processed_data",
               "euler",
               "all12su.csv"))

write_csv(c,
          here("Processed_data",
               "euler",
               "alltrawl.csv"))









