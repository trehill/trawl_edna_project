#Taxonomic Assignments 
#Authors: Tessa Rehill and Ben Millard-Martin 

#read data ####
# assign taxonomy
# create taxa_by_site_survey matrix
# plot some summaries

# packages and data ####
library(tidyverse)
library(RColorBrewer)
library(here)
library(vegan)
library(usedist)
library(taxize)
library(janitor)
library(dplyr)

#LCT assignments for 12se data ####

#read 12se data 

ASVbysite <- read.csv(here::here("Processed_data", #should be ASV by sample
                                   "eDNA",
                                   "12s",
                                   "12s_e",
                                  "asv",
                                   "matrix",
                                    "clean_data",
                                   "data12Se_asvmatrix_nc_lor_nfc.csv"),
                        head=TRUE)

#sitesurvey_data <- readRDS("Data/2022_10_31/derived_data/sitesurvey_data.rds") #no equivalent for my data 

sample_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "metadata",
                                 "clean_data",
                                 "eDNA_metadata.csv"),
                      head=TRUE)


LCA_method <- read.csv(here::here("Raw_data", #should be ASV by sample
                                  "eDNA",
                                  "12s",
                                  "12s_e",
                                  "MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.LCA_ONLY.txt"),
                       head=TRUE)

best_hit <-  read.csv(here::here("Raw_data", #should be ASV by sample
                                 "eDNA",
                                 "12s",
                                 "12s_e",
                                 "MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.txt"),
                      head=TRUE)

top10 <- read.delim("Raw_data/eDNA/12s/12s_e/MiFish_E_12S_ASV_sequences.length_var.blast.out",
                    h=TRUE,
                    fill = TRUE) %>%
  `colnames<-`(c("ASV", "subject", "accesion_num", "taxa_ID", "perc_ID", "coverage", "evalue", "bitscore", "source", "taxonomy")) %>%
  as.data.frame() %>%
  na.exclude() %>%
  separate(taxonomy, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = " / ") %>%
  filter(class == "Actinopteri" | class == "Chondrichthyes") 


#select ASVs that passed occupancy models
top10_occ <- top10 %>%
  filter(ASV %in% colnames(ASVbysite))

#identify max percent ID for each ASV 
max_ID <- top10_occ %>%
  group_by(ASV) %>%
  summarise(perc_ID = max(perc_ID))

#select taxonomy for max percent ID
f1 <- merge(max_ID, top10_occ, by = c("ASV", "perc_ID"))

#taxize #### only run once and turn off with "#" (as below) 
spec_unique <- unique(f1$species)

#check accepted naming and get higher taxonomy
#top10_gbifid_higher <- classification(sci_id = spec_unique,
#                                                db = 'gbif',
#                                                #give back ID
#                                                 return_id = TRUE) %>%
  #bind them together
 # cbind(.) 

names(top10_gbifid_higher)[names(top10_gbifid_higher) == 'query'] <- 'ids' #rename query column to ids

#write_csv(top10_gbifid_higher,
#         here("Processed_data",
#             "eDNA",
#               "12s",
#               "12s_e",
#             "LCTassignment",
#              "top10_gbifid_higher.csv"))

top10_gbifid_higher <- read.csv(here::here("Processed_data",
                                                         "eDNA",
                                                          "12s",
                                                          "12s_e",
                                                          "LCTassignment",
                                                         "top10_gbifid_higher.csv"))
# merge with old names
new_taxonomy <- top10_gbifid_higher %>%
  mutate(old_species = spec_unique) %>%
  relocate(old_species, .after = species)

##################################manual editing required on next lines###########################################

write_csv(new_taxonomy,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "LCTassignment",
               "12setaxonomy.csv"))

#export and annotate: add "in_range" column in excell and annotate "y" if in 
#northeast pacific or tributaries, "n" if from other oceans (including western pacific)
#search in FishBase
#rename as below
#Raja binoculata --> does not come up in Fishbase, has different name than one listed (common name: Big Skate)
      #it's just had a name change - now it's "Beringraja binoculata"
#Sebastes cheni --> not in FishBase, searched for it on google
#could do this by comparing to Matt's species list instead 

new_taxa <- read.csv(here::here("Processed_data",
                                "eDNA",
                                "12s",
                                "12s_e",
                                "LCTassignment",
                                "12setaxonomy2.csv"),
                     head=TRUE)  #import annotated taxa

#create dataframe with all top hits and accepted taxonomy 
#select tazonomy for max percent ID
f2 <- merge(f1[c("ASV", "perc_ID", "species")], new_taxa[1:17], by.x = "species", by.y = "ids") %>% #might need to change number inside []
      select(-c("species")) %>%
      rename("species" = "species.y")
#count the numbers of families, genera, and species with equal max percent ID
f3 <- f2 %>%
  group_by(ASV, perc_ID) %>%
  summarise(across(c("family", "genus", "species"), ~ length(unique(.x)))) %>%
  `colnames<-`(c("ASV", "perc_ID", "fam_n", "gen_n", "spec_n"))

#group ASV to groups that need to be collapsed to family or within and genera or within
fam <- filter(f3, gen_n >= 2)
gen <- filter(f3, gen_n == 1 & spec_n > 1)
spec <- filter(f3, spec_n == 1)

#sort out multiple hits within family ####
#list groups where >2 species
fam_tax <- merge(fam, f2, by = c("ASV", "perc_ID")) %>%
  distinct() 

#which groups have multiple species in range
r1 <- fam_tax %>%
  group_by(ASV) %>%
  summarize(n_in_range = sum(in_range == "y"))

r2 <- filter(r1, n_in_range == 1) 
d1 <- filter(fam_tax, ASV %in% r2$ASV) %>%      #ASVs with multiple hits, but only one species assignment in range, complete
  filter(in_range == "y") %>%
  mutate(LCT = species)%>%
  mutate(all_species = species) %>%
  mutate(level = "species") %>%
  .[c("ASV", "level", "LCT", "class", "order", "family", "genus", "species", "all_species")]
r3 <- filter(r1, n_in_range > 1)                #when multiple species in range (not our case!)
r4 <- filter(fam_tax, ASV %in% r3$ASV) %>%
  filter(in_range == "y")
r5 <- r4 %>%
  group_by(ASV) %>%
  summarise(p1 = length(unique(genus))) %>%
  filter(p1 == 1)
r6 <- filter(r4, ASV %in% r5$ASV)               #ASVs with multiple species within genera, add to gen_tax below to cluster within genera
r7 <- filter(r4, !ASV %in% r5$ASV)              #ASVs with multiple genera within families, assign grouping manually
r8 <- r7[with(r7, order(ASV, species)), ] %>%
  group_by(ASV, class, order, family, in_range) %>%
  summarise(all_species = paste(species, collapse=", "))

##################################manual editing required on next lines###########################################
#r9 <- data.frame(all_species = unique(r8$all_species))                         #table of family groups
#r9$LCT <- c("Pleuronectidae1")                            #add a group name (in order) for each row in r9
#r10 <- merge(r8, r9, by = "all_species") %>%
#  mutate(level = "family") %>%
#  mutate(genus = LCT) %>%
#  mutate(species = LCT) %>%
#  distinct()
#d2 <- r10 %>%
#  .[c("ASV", "level", "LCT", "class", "order", "family", "genus", "species", "all_species")]

# merger q4, q5, q6 for table
#tab_fam <- merge(r7[c("ASV", "species")], r8[c("ASV", "all_species")], by = "ASV") %>%
#  merge(., r9, by = "all_species") %>%
#  select(!ASV) %>%
#  distinct()

#sort out multiple hits within genera ####
gen_tax <- merge(gen, f2, by = c("ASV", "perc_ID")) %>%
  distinct() %>%
  rbind(.,r6)                                   #add ASVs with multiple species within genera from above
#which groups have multiple species in range
q1 <- gen_tax %>%
  group_by(ASV) %>%
  summarize(n_in_range = sum(in_range == "y"))
q2 <- filter(q1, n_in_range == 1) 
d3 <- filter(gen_tax, ASV %in% q2$ASV) %>%      #ASVs with multiple hits, but only one species assignment in range, complete
  filter(in_range == "y")%>%
  mutate(LCT = species)%>%
  mutate(level = "species")%>%
  mutate(all_species = species) %>%
  .[c("ASV", "level", "LCT", "class", "order", "family", "genus", "species", "all_species")]
q3 <- filter(q1, n_in_range > 1)                #when multiple species in range
q4 <- filter(gen_tax, ASV %in% q3$ASV) %>%
  filter(in_range == "y")                       #ASVs with multiple species in region, in a genus, assign to group below
q5 <- q4[with(q4, order(ASV, species)), ] %>%
  group_by(ASV, class, order, family, genus, in_range) %>%
  summarise(all_species = paste(species, collapse=", "))
q6 <- data.frame(all_species = unique(q5$all_species))                         #table of genus groups
##################################manual editing required on next lines########################################### 
q6$LCT <- c("Sebastes caurinus/maliger")         #add a group name (in order) for each row in q6
q7 <- merge(q5, q6, by = "all_species") %>%
  mutate(level = "genus") %>%
  mutate(species = LCT)


d4 <- q7 %>%
  .[c("ASV", "level", "LCT", "class", "order", "family", "genus", "species", "all_species")]

# merger q4, q5, q6 for table
tab_gen <- merge(q4[c("ASV", "species")], q5[c("ASV", "all_species")], by = "ASV") %>%
  merge(., q6, by = "all_species") %>%
  select(!ASV) %>%
  distinct()

##################################manual editing MAY BE required on next lines########################################### 
#species level issues: out of range ####
spec_tax <- merge(spec, f2, by = c("ASV", "perc_ID"))%>%
  distinct()
y1 <- spec_tax[c(1:21)] #changed these brackets from Ben's code... not sure if it will cause issues 
y2 <- y1 %>%                              #species outside of range, assign taxa by next best hit
  filter(in_range != "y") %>%
  mutate(LCT = c("Reinhardtius evermanni"))%>%             #new assignment goes here (between "")
mutate(level = c("species")) %>%             #level of assignment goes here (between "") 
mutate(in_range = c("y"))%>%         
  mutate(all_species = LCT)%>%         
  mutate(species = LCT)
y3 <- filter(y2, level == "genus") %>%
  mutate(all_species = species) %>%
  mutate(species = LCT)
y4 <- filter(y2, level == "family") %>%
  mutate(all_species = species) %>%
  mutate(species = species) %>%
  mutate(genus = LCT)
y5 <- filter(y2, level == "species") %>%
  mutate(all_species = species)%>%
  mutate(species = LCT) %>%
  mutate(genus = word(LCT,1)) #extract genus from species name
y6 <- filter(y2, level == "class") %>%
  mutate(all_species = species) %>%
  mutate(species = LCT) %>%
  mutate(genus = LCT) %>%
  mutate(family = LCT) %>%
  mutate(order = LCT) 
y7 <- rbind(y3,y4,y5,y6)      # NOTE some of y3-y6 don't do anything now,but may when we have other ID issues
d5 <- y7 %>%
  .[c("ASV", "level", "LCT", "class", "order", "family", "genus", "species", "all_species")]


y8 <- y1 %>%                              #species inside of range, with single hits, or family or genus issues resolved by removing range issues
  filter(in_range == "y") %>%
  .[c("ASV","species", "class", "order", "family", "genus")] %>%
  rbind(.,d1[c("ASV","species", "class", "order", "family", "genus")]) %>%               #add species from family errors that were resolved by removing range issues
  rbind(.,d3[c("ASV","species", "class", "order", "family", "genus")])               #add species from genus errors that were resolved by removing range issues

# species that were assigned to groups for some ASVs but not others
#y9 <- rbind(tab_fam, tab_gen) #grouping table #Ben's code doesn't work because we didn't have any family nuances 
y9 <- rbind(tab_gen) #grouping table #doesn't work because we didn't have any family nuances 
y10 <- merge(y8, y9, by = "species", all.x = T) 
y11 <- filter(y10, !is.na(y10$all_species)) %>%       #the ones that slipped through
  mutate(level = "genus")  %>%       #the ones that slipped through
  mutate(species = LCT)                                   #assign manually
y12 <- filter(y10, is.na(y10$all_species)) %>%
  mutate(level = "species") %>%
  mutate(LCT = species) %>%
  mutate(all_species = species)
d6 <- rbind(y12,y11)%>%
  .[c("ASV", "level", "LCT", "class", "order", "family", "genus", "species", "all_species")]

#data <- rbind(d6, d5, d4, d2) #Ben's code doesn't work because d2 doesn't exists (no family nuances)
data <- rbind(d6, d5, d4)

#check that all ASVs are accounted for and not duplicated - they should all be the same
length(data$ASV) #52 *now 53
length(unique(data$ASV)) #52*now 53
length(unique(top10_occ$ASV)) #55 

#check which are missing

unique(top10_occ$ASV)[!unique(top10_occ$ASV) %in% unique(data$ASV)]

table <- data[c("level", "LCT", "class", "order", "family", "genus", "species", "all_species")] %>%
  distinct()

write_csv(table,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "LCTassignment",
               "taxonomy_groups_12s_eDNA.csv"))


write_csv(data,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "LCTassignment",
               "ASV_taxonomy_12seDNA.csv"))


#make LCT by site matrix ####

LCT_by_site <- ASVbysite %>%
  column_to_rownames("original_sample_name") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column( var = "ASV") %>%
  merge(data[c("ASV", "LCT")], ., by = "ASV", all.x = T) 

LCT_by_site <- subset(LCT_by_site, select = -ASV )

LCT_by_site <- LCT_by_site %>%
  group_by(LCT) %>% 
  summarise(across(.cols = everything(), ~sum(.x))) %>%
  column_to_rownames(var = "LCT") %>%
  t()%>%
  as.data.frame()


write_csv(LCT_by_site,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "LCTassignment",
               "LCT_by_site12se.csv"))

#LCT assignment for 12su ####
ASVbysite <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "12s",
                                 "12s_u",
                                 "asv",
                                 "matrix",
                                 "clean_data",
                                 "data12Su_asvmatrix_nc_lor_nfc.csv"),
                      head=TRUE)


LCA_method <- read.csv(here::here("Raw_data", #should be ASV by sample
                                  "eDNA",
                                  "12s",
                                  "12s_u",
                                 "MiFish_U_taxonomy_table.12S.NCBI_NT.96sim.LCA_ONLY.txt"),
                       head=TRUE)

best_hit <-  read.csv(here::here("Raw_data", #should be ASV by sample
                                 "eDNA",
                                 "12s",
                                 "12s_u",
                                 "MiFish_U_taxonomy_table.12S.NCBI_NT.96sim.txt"),
                      head=TRUE)

top10 <- read.delim("Raw_data/eDNA/12s/12s_u/MiFish_U_12S_ASV_sequences.length_var.blast.out",
                    h=TRUE,
                    fill = TRUE) %>%
  `colnames<-`(c("ASV", "subject", "accesion_num", "taxa_ID", "perc_ID", "coverage", "evalue", "bitscore", "source", "taxonomy")) %>%
  as.data.frame() %>%
  na.exclude() %>%
  separate(taxonomy, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = " / ") %>%
  filter(class == "Actinopteri" | class == "Chondrichthyes") 


#select ASVs that passed occupancy models
top10_occ <- top10 %>%
  filter(ASV %in% colnames(ASVbysite))

#identify max percent ID for each ASV 
max_ID <- top10_occ %>%
  group_by(ASV) %>%
  summarise(perc_ID = max(perc_ID))

#select taxonomy for max percent ID
f1 <- merge(max_ID, top10_occ, by = c("ASV", "perc_ID"))

#taxize #### only run once and turn off with "#" (as below) 
spec_unique <- unique(f1$species)

#check accepted naming and get higher taxonomy
#top10_gbifid_higher <- classification(sci_id = spec_unique,
 #                                                 db = 'gbif',
                                                 #give back ID
#                                                return_id = TRUE) %>%
#bind them together
#  cbind(.) 

#names(top10_gbifid_higher)[names(top10_gbifid_higher) == 'query'] <- 'ids' #rename query column to ids

#write_csv(top10_gbifid_higher,
#         here("Processed_data",
#             "eDNA",
 #              "12s",
#               "12s_u",
 #              "LCTassignment",
#               "top10_gbifid_higher.csv"))

top10_gbifid_higher <- read.csv(here::here("Processed_data",
                                           "eDNA",
                                           "12s",
                                           "12s_u",
                                           "LCTassignment",
                                           "top10_gbifid_higher.csv"))
# merge with old names
new_taxonomy <- top10_gbifid_higher %>%
  mutate(old_species = spec_unique) %>%
  relocate(old_species, .after = species)

##################################manual editing required on next lines###########################################

write_csv(new_taxonomy,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "LCTassignment",
               "12sutaxonomy.csv"))

#export and annotate: add "in_range" column in excell and annotate "y" if in 
#northeast pacific or tributaries, "n" if from other oceans (including western pacific)
#search in FishBase
#rename as below
#Raja binoculata --> does not come up in Fishbase, has different name than one listed (common name: Big Skate)
#it's just had a name change - now it's "Beringraja binoculata"
#Sebastes cheni --> not in FishBase, searched for it on google
#could do this by comparing to Matt's species list instead 

new_taxa <- read.csv(here::here("Processed_data",
                                "eDNA",
                                "12s",
                                "12s_u",
                                "LCTassignment",
                                "12sutaxonomy3.csv"),
                     head=TRUE)  #import annotated taxa

#create dataframe with all top hits and accepted taxonomy 
#select tazonomy for max percent ID
f2 <- merge(f1[c("ASV", "perc_ID", "species")], new_taxa[1:15], by.x = "species", by.y = "query") %>% #might need to change number inside []
  select(-c("species")) %>%
  rename("species" = "species.y")

#count the numbers of families, genera, and species with equal max percent ID
f3 <- f2 %>%
  group_by(ASV, perc_ID) %>%
  summarise(across(c("family", "genus", "species"), ~ length(unique(.x)))) %>%
  `colnames<-`(c("ASV", "perc_ID", "fam_n", "gen_n", "spec_n"))

#group ASV to groups that need to be collapsed to family or within and genera or within
fam <- filter(f3, gen_n >= 2)
gen <- filter(f3, gen_n == 1 & spec_n > 1)
spec <- filter(f3, spec_n == 1)

#sort out multiple hits within family ####
#list groups where >2 species
fam_tax <- merge(fam, f2, by = c("ASV", "perc_ID")) %>%
  distinct() 

#which groups have multiple species in range
r1 <- fam_tax %>%
  group_by(ASV) %>%
  summarize(n_in_range = sum(in_range == "y"))

r2 <- filter(r1, n_in_range == 1) 
d1 <- filter(fam_tax, ASV %in% r2$ASV) %>%      #ASVs with multiple hits, but only one species assignment in range, complete
  filter(in_range == "y") %>%
  mutate(LCT = species)%>%
  mutate(all_species = species) %>%
  mutate(level = "species") %>%
  .[c("ASV", "level", "LCT", "order", "family", "genus", "species", "all_species")]
r3 <- filter(r1, n_in_range > 1)                #when multiple species in range (not our case!)
r4 <- filter(fam_tax, ASV %in% r3$ASV) %>%
  filter(in_range == "y")
r5 <- r4 %>%
  group_by(ASV) %>%
  summarise(p1 = length(unique(genus))) %>%
  filter(p1 == 1)
r6 <- filter(r4, ASV %in% r5$ASV)               #ASVs with multiple species within genera, add to gen_tax below to cluster within genera
r7 <- filter(r4, !ASV %in% r5$ASV)              #ASVs with multiple genera within families, assign grouping manually
r8 <- r7[with(r7, order(ASV, species)), ] %>%
  group_by(ASV, order, family, in_range) %>%
  summarise(all_species = paste(species, collapse=", "))

##################################manual editing required on next lines###########################################
#r9 <- data.frame(all_species = unique(r8$all_species))                         #table of family groups
#r9$LCT <- c("Pleuronectidae1")                            #add a group name (in order) for each row in r9
#r10 <- merge(r8, r9, by = "all_species") %>%
#  mutate(level = "family") %>%
#  mutate(genus = LCT) %>%
#  mutate(species = LCT) %>%
#  distinct()
#d2 <- r10 %>%
#  .[c("ASV", "level", "LCT", "class", "order", "family", "genus", "species", "all_species")]

# merger q4, q5, q6 for table
#tab_fam <- merge(r7[c("ASV", "species")], r8[c("ASV", "all_species")], by = "ASV") %>%
#  merge(., r9, by = "all_species") %>%
#  select(!ASV) %>%
#  distinct()

#no multiple hits within family (all r dataframes = 0)

#sort out multiple hits within genera ####
gen_tax <- merge(gen, f2, by = c("ASV", "perc_ID")) %>%
  distinct() %>%
  rbind(.,r6)                                   #add ASVs with multiple species within genera from above
#which groups have multiple species in range
q1 <- gen_tax %>%
  group_by(ASV) %>%
  summarize(n_in_range = sum(in_range == "y"))
q2 <- filter(q1, n_in_range == 1) 
d3 <- filter(gen_tax, ASV %in% q2$ASV) %>%      #ASVs with multiple hits, but only one species assignment in range, complete
  filter(in_range == "y")%>%
  mutate(LCT = species)%>%
  mutate(level = "species")%>%
  mutate(all_species = species) %>%
  .[c("ASV", "level", "LCT", "order", "family", "genus", "species", "all_species")]
q3 <- filter(q1, n_in_range > 1)                #when multiple species in range
q4 <- filter(gen_tax, ASV %in% q3$ASV) %>%
  filter(in_range == "y")                       #ASVs with multiple species in region, in a genus, assign to group below
q5 <- q4[with(q4, order(ASV, species)), ] %>%
  group_by(ASV, order, family, genus, in_range) %>%
  summarise(all_species = paste(species, collapse=", "))
q6 <- data.frame(all_species = unique(q5$all_species))                         #table of genus groups
##################################manual editing required on next lines########################################### 
q6$LCT <- c("Sebastes caurinus/maliger", "Xiphister atropurpureus/mucosus")         #add a group name (in order) for each row in q6

q7 <- merge(q5, q6, by = "all_species") %>%
  mutate(level = "genus") %>%
  mutate(species = LCT)


d4 <- q7 %>%
  .[c("ASV", "level", "LCT", "order", "family", "genus", "species", "all_species")]

# merger q4, q5, q6 for table
tab_gen <- merge(q4[c("ASV", "species")], q5[c("ASV", "all_species")], by = "ASV") %>%
  merge(., q6, by = "all_species") %>%
  select(!ASV) %>%
  distinct()

##################################manual editing MAY BE required on next lines########################################### 
#species level issues: out of range ####
spec_tax <- merge(spec, f2, by = c("ASV", "perc_ID"))%>%
  distinct()
y1 <- spec_tax[c(1:19)] #changed these brackets from Ben's code... not sure if it will cause issues 
#one species out of range = Gadus morhua 
y2 <- y1 %>%                              #species outside of range, assign taxa by next best hit
  filter(in_range != "y") %>%
  mutate(LCT = c("Gadus chalcogrammus"))%>%             #new assignment goes here (between "")
  mutate(level = c("species")) %>%             #level of assignment goes here (between "") 
  mutate(in_range = c("y"))%>%         
  mutate(all_species = LCT)%>%         
  mutate(species = LCT)
y3 <- filter(y2, level == "genus") %>%
  mutate(all_species = species) %>%
  mutate(species = LCT)
y4 <- filter(y2, level == "family") %>%
  mutate(all_species = species) %>%
  mutate(species = species) %>%
  mutate(genus = LCT)
y5 <- filter(y2, level == "species") %>%
  mutate(all_species = species)%>%
  mutate(species = LCT) %>%
  mutate(genus = word(LCT,1)) #extract genus from species name
y6 <- filter(y2, level == "class") %>%
  mutate(all_species = species) %>%
  mutate(species = LCT) %>%
  mutate(genus = LCT) %>%
  mutate(family = LCT) %>%
  mutate(order = LCT) 
y7 <- rbind(y3,y4,y5,y6)      # NOTE some of y3-y6 don't do anything now,but may when we have other ID issues
d5 <- y7 %>%
  .[c("ASV", "level", "LCT", "order", "family", "genus", "species", "all_species")]


y8 <- y1 %>%                              #species inside of range, with single hits, or family or genus issues resolved by removing range issues
  filter(in_range == "y") %>%
  .[c("ASV","species","order", "family", "genus")] %>%
  rbind(.,d1[c("ASV","species", "order", "family", "genus")]) %>%               #add species from family errors that were resolved by removing range issues
  rbind(.,d3[c("ASV","species",  "order", "family", "genus")])               #add species from genus errors that were resolved by removing range issues

# species that were assigned to groups for some ASVs but not others
#y9 <- rbind(tab_fam, tab_gen) #grouping table #Ben's code doesn't work because we didn't have any family nuances 
y9 <- rbind(tab_gen) #grouping table #doesn't work because we didn't have any family nuances 
y10 <- merge(y8, y9, by = "species", all.x = T) 
y11 <- filter(y10, !is.na(y10$all_species)) %>%       #the ones that slipped through
  mutate(level = "genus")  %>%       #the ones that slipped through
  mutate(LCT = species)                                   #assign manually
y12 <- filter(y10, is.na(y10$all_species)) %>%
  mutate(level = "species") %>%
  mutate(LCT = species) %>%
  mutate(all_species = species)
d6 <- rbind(y12,y11)%>%
  .[c("ASV", "level", "LCT", "order", "family", "genus", "species", "all_species")]

#data <- rbind(d6, d5, d4, d2) #Ben's code doesn't work because d2 doesn't exists (no family nuances)
data <- rbind(d6, d5, d4)

#check that all ASVs are accounted for and not duplicated - they should all be the same
length(data$ASV) #36
length(unique(data$ASV)) #36
length(unique(top10_occ$ASV)) #36

#check which are missing

unique(top10_occ$ASV)[!unique(top10_occ$ASV) %in% unique(data$ASV)]

table <- data[c("level", "LCT", "order", "family", "genus", "species", "all_species")] %>%
  distinct()

write_csv(table,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "LCTassignment",
               "taxonomy_groups_12u_eDNA.csv"))


write_csv(data,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "LCTassignment",
               "ASV_taxonomy_12suDNA.csv"))


#now merge both 12se and 12su data 

#make LCT by site matrix ####

LCT_by_site <- ASVbysite %>%
  column_to_rownames("original_sample_name") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column( var = "ASV") %>%
  merge(data[c("ASV", "LCT")], ., by = "ASV", all.x = T) 

LCT_by_site <- subset(LCT_by_site, select = -ASV )

LCT_by_site <- LCT_by_site %>%
  group_by(LCT) %>% 
  summarise(across(.cols = everything(), ~sum(.x))) %>%
  column_to_rownames(var = "LCT") %>%
  t()%>%
  as.data.frame()


write_csv(LCT_by_site,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "LCTassignment",
               "LCT_by_site12su.csv"))



