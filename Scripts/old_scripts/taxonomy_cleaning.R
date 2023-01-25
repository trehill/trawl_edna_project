### Goal = clean taxonomic data
### Authors = Anya Mueller 
### Date = ENTER DATE HERE
### Edited by Tessa Rehill 

# Set up ------------------------------------------------------------------
## install packages
# install the following packages if needed
#install.packages("tidyverse")
#install.packages("here")
#install.packages("janitor")
#install.packages("taxize")

## read in packages
library(tidyverse)
library(here)
library(janitor)
library(taxize)

## read in land animal lists
land_animals_class <- c('Collembola', 'Insecta', 'Amphibia', 'Japygidae',
                        'Diplopoda', 'Chilopoda', 'Aves', 'Lepidosauria', 'Kinosternidae',
                        'Emydidae', 'Metazoa incertae sedis', 'Arthropoda incertae sedis',
                        'Chordata incertae sedis', 'Testudinidae', 'Parajapygidae',
                        'Alligatoridae', 'Trionychidae', 'Chelidae', 'Octostigmatidae',
                        'Geoemydidae', 'Crocodylidae', 'Podocnemididae', 'Pelomedusidae',
                        'Udeonychophora', 'Dermatemydidae', 'Dermochelyidae', 'Chelydridae')
land_animals_family <- c('Rodentia', 'Canidae', 'Felidae', 'Ursidae', 'Cervidae',
                         'Hominidae', 'Procavidae', 'Vespertilionidae', 'Rhinolophidae',
                         'Soricidae', 'Erinaceidae', 'Talpidae', 'Macropodidae', 
                         'Suidae', 'Procyonidae', 'Daubentoniidae', 'Lemuridae', 
                         'Tarsiidae', 'Muridae', 'Bovidae', 'Nesomyidae', 'Camelidae',
                         'Equidae', 'Geomyidae', 'Sciuridae', 'Mustelidae', 'Caviidae',
                         'Hippopotamidae', 'Elephantidae', 'Vombatidae', 'Tenrecidae',
                         'Thryonomyidae', 'Spalacidae', 'Macroscelididae', 'Tupaiidae', 
                         'Manidae', 'Cynocephalidae', 'Chrysochloridae', 'Didelphidae',
                         'Cricetidae', 'Procaviidae', 'Phalangeridae', 'Tachyglossidae',
                         'Ailuridae', 'Pseudocheiridae', 'Peramelidae', 'Potoroidae', 
                         'Tayassuidae', 'Heteromyidae', 'Hylobatidae', 'Cebidae', 'Galagidae',
                         'Aotidae', 'Cercopithecidae', 'Burramyidae', 'Ochotonidae',
                         'Pteropodidae', 'Mephitidae', 'Tapiridae', 'Dasyuridae',
                         'Phyllostomidae', 'Molossidae', 'Emballonuridae', 'Mormoopidae',
                         'Echimyidae', 'Rhinocerotidae', 'Viverridae', 'Anomaluridae',
                         'Hystricidae', 'Atelidae', 'Antilocapridae', 'Bathyergidae',
                         'Bradypodidae', 'Caenolestidae', 'Caenolestidae', 'Calomyscidae',
                         'Capromyidae', 'Castoridae', 'Cheirogaleidae', 'Tragulidae',
                         'Thyropteridae', 'Thylacinidae', 'Solenodontidae', 'Rhinopomatidae',
                         'Rhinonycteridae', 'Platacanthomyidae', 'Pitheciidae', 'Ctenomyidae',
                         'Chlamyphoridae', 'Chiroptera', 'Chinchillidae', 'Cuniculidae',
                         'Cyclopedidae', 'Dasypodidae', 'Dasyproctidae', 'Dipodidae',
                         'Erethizontidae', 'Giraffidae', 'Gliridae', 'Furipteridae',
                         'Eupleridae', 'Herpestidae', 'Hipposideridae', 'Hyaenidae',
                         'Indriidae', 'Petauridae', 'Peroryctidae', 'Paleopropithecidae',
                         'Orycteropodidae', 'Ornithorhynchidae', 'Octodontidae', 'Nycteridae',
                         'Noctilionidae', 'Natalidae', 'Myrmecophagidae', 'Nandiniidae',
                         'Myocastoridae', 'Moschidae', 'Megalonychidae', 'Mammalia incertae sedis',
                         'Dictynidae', 'Lycosidae', 'Philodromidae', 'Salticidae', 'Tetragnathidae',
                         'Linyphiidae', 'Thomisidae', 'Pisauridae', 'Tetranychidae', 'Agelenidae',
                         'Araneidae', 'Pettalidae', 'Sironidae', 'Neogoveidae', 'Troglosironidae',
                         'Buthidae', 'Euctenizidae', 'Phalangiidae', 'Heptathelidae',
                         'Liphistiidae', 'Anyphaenidae', 'Phalangodidae', 'Epedanidae',
                         'Gnaphosidae', 'Ctenidae',
                         'Thelyphonidae', 'Dysderidae', 'Antrodiaetidae', 'Stylocellidae',
                         'Pholcidae', 'Amaurobiidae', 'Theridiidae', 'Clubionidae', 'Miturgidae',
                         'Diguetidae')


# 12Se --------------------------------------------------------------------
## read in files ----
data12se_taxonomy <- read.delim(here("Raw_data",
                                   "eDNA",
                                   "12s",
                                   "12s_e",
                                   "taxonomy", #for some reason it doesn't like the file living in this folder???
                                   "MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.txt"),
                                h=TRUE,
                                fill = TRUE)

## clean up column names ----
colnames(data12se_taxonomy) <- data12se_taxonomy %>%
  colnames(.) %>% 
  str_remove_all(string = .,
                 pattern = "X.") #remove X. from column names
data12se_taxonomy <- data12se_taxonomy %>% 
  #clean up column names
  clean_names() 

## clean up data content ----
data12se_taxonomy <- data12se_taxonomy %>%
  # remove anything that contains "unknown"
  mutate(across(.cols = everything(),
                .fns = ~ str_replace_all(string = .,
                                     pattern = "unknown.*",
                                     replacement = NA_character_))
         ) %>%
  # remove "no identification"
  mutate(across(.cols = everything(),
                .fns = ~ str_replace_all(string = .,
                                         pattern = "^no identification$",
                                         replacement = NA_character_))
  ) %>%
  # remove anything that contains "uncultured"
  mutate(across(.cols = everything(),
                .fns = ~ str_replace_all(string = .,
                                         pattern = "uncultured.*",
                                         replacement = NA_character_))
  ) %>%
  # filter out "filtered out" hits
  filter(species != "filtered out")

## clean up taxonomic assignment ----
#strip higher taxonomy and repopulate with correct assignments
# resolve species names
data12se_species_resolved <- tol_resolve(data12se_taxonomy %>%
                                           #remove nas
                                           drop_na(species) %>%
                                           #take species vector
                                           pull(species) %>% #length() - 606 asvs identified to species
                                           #take distinct values
                                           unique(.) #%>% length() - 87 unique species identified
                                         )

data12se_species_resolved %>%
  pull(is_synonym) %>%
  unique() #none are synonyms so use original names

# get gbif ids for the scientific names
data12se_species_gbifid <- get_gbifid(sci = data12se_taxonomy %>%
                                        #remove nas
                                        drop_na(species) %>%
                                        #take species vector
                                        pull(species) %>% #length() - 601 asvs identified to species
                                        #take distinct values
                                        unique(.), #%>% length() - 82 unique species identified
                                      #run interactively - if more than one id is found, ask me to chose - check taxonomy_decisions.txt for decisions made
                                      # input 1 each time it asks
                                      ask = TRUE) #found 82/82 gbifids
#make into a dataframe
data12se_species_gbifid_df <- data12se_species_gbifid %>% 
  as.data.frame(.) %>%
  #add in species info
  mutate(species = data12se_taxonomy %>%
           #remove nas
           drop_na(species) %>%
           #take species vector
           pull(species) %>% #length() - 606 asvs identified to species
           #take distinct values
           unique(.)
         )

#get higher taxonomy
data12se_species_gbifid_higher <- classification(sci_id = data12se_species_gbifid_df %>%
                                                   #filter out the ones not found
                                                   filter(match == "found") %>%
                                                   pull(ids) %>%
                                                   unique(), #%>% length(), - 82 ids
                                                 db = 'gbif',
                                                 #give back ID
                                                 return_id = TRUE) %>%
  #bind them together
  cbind(.) %>%
  #remove classifications that didn't reach the species level
  drop_na(species)

data12se_species_gbifid_higher %>% pull(species) %>% unique() %>% length() #80 species

# reformat table
data12se_species_gbifid_higher <- data12se_species_gbifid_higher %>% 
  # rename query row
  rename("gbif_query" = "query") %>% 
  #add in asv associated with each species - right join because there can be more than one
  right_join(., 
             data12se_taxonomy %>%
               #take columns of interest
               select(species,
                      query) %>%
               #remove rows containing NA
               drop_na() %>%
               #merge with gbifid
               full_join(.,
                         data12se_species_gbifid_df %>% 
                           #select columns of interest
                           select(species,
                                  ids) %>%
                           #rename column for later merge
                           rename("gbif_query" = "ids")
                           ) %>%
               #take columns for merge
               select(gbif_query, 
                      query),
             by = "gbif_query") %>%
  #drop rows of species not found by gbif
  drop_na(species_id) %>%
  #add indicator for level of query
  mutate(taxo_query_level = "species") %>%
  #add query source database
  mutate(higher_taxo_source = "gbif") %>%
  #clean column names
  clean_names()

data12se_species_gbifid_higher %>% pull(species) %>% unique() %>% length() #80
data12se_species_gbifid_higher %>% pull(query) %>% unique() %>% length() #598

# check what species didn't get an gbif identification
no_species_id_gbif <- data12se_taxonomy %>%
  filter(!query %in% c(data12se_species_gbifid_higher %>%
                         pull(query)
                       )
         )

# look at the species against worms 
data12se_species_worms <- get_wormsid(sci_com = no_species_id_gbif %>%
                                        drop_na(species) %>%
                                        pull(species) %>%
                                        unique(.), #%>% length() - 2 unique species
                                      #run in interactive mode
                                      ask = TRUE) #neither were found

no_species_id_gbif %>% pull(species) %>% unique() #"Myzopsetta ferruginea", "alpha proteobacterium SCGC AAA076-E09"
#^ these two names will be filtered out

# now lets look at genus that haven't been fixed at species level
no_species_id <- data12se_taxonomy %>%
  filter(!query %in% c(data12se_species_gbifid_higher %>%
                         pull(query)
                       )
         ) %>%
  #remove species
  select(-species) %>%
  #remove unavailable genus
  drop_na(genus)

no_species_id %>% pull(genus) %>% unique() %>% length() # 1

data12se_genus_resolved <- tol_resolve(no_species_id %>%
                                    #take species vector
                                    pull(genus) %>%
                                    #take distinct values
                                    unique(.))
data12se_genus_resolved %>%
  pull(is_synonym) %>%
  unique() #none are synonyms, so we can use the ones we have (some of the resolved names are weird)

#add in higher taxonomy that has already been found by gbif
data12se_genus_higher <- no_species_id %>%
  #take only genus and ASV id
  select(genus, 
         query) %>%
  #merge with higher taxonomy
  left_join(.,
            data12se_species_gbifid_higher %>%
              #remove lower taxo levels than genus
              select(-starts_with("species"),
                     -starts_with("subspecies"),
                     -query,
                     -taxo_query_level,
                     -gbif_query) %>%
              #take distinct rows
              distinct()) %>%
  #remove ones not resolved
  drop_na(genus_id) %>%
  #add indicator of which taxonomic level the higher taxonomy was determined from
  mutate(taxo_query_level = "genus") # there was no overlap

# get species for the genus so can pull higher taxonomy
unresolved_spe_gbif <- downstream(sci_id = no_species_id %>%
                                    pull(genus) %>%
                                    unique(),
                                  db = "gbif",
                                  #only return down to species
                                  downto = "species",
                                  #only give back 1 species for each genus
                                  limit = 1)
unresolved_spe_gbif$Myzopsetta #does not exist - check in worms

unresolved_spe_worms <- downstream(sci_id = no_species_id %>%
                                    pull(genus) %>%
                                    unique(),
                                  db = "worms",
                                  #only return down to species
                                  downto = "species",
                                  #only give back 1 species for each genus
                                  limit = 1)
unresolved_spe_worms$Myzopsetta
#get higher taxonomy

data12se_genus_higher_worms <- classification(sci_id = unresolved_spe_worms$Myzopsetta %>%
                                                #take first row
                                                slice_head() %>%
                                                #take species names
                                                pull(name),
                                              db = 'worms',
                                              #give back ID
                                              return_id = TRUE) %>%
  #bind them together
  cbind(.) %>% 
  #strip info lower than species
  select(-starts_with("species")) %>%
  #take distinct rows
  distinct()

data12se_genus_higher_worms_df <- data12se_genus_higher_worms %>%
  as.data.frame()

#reformat table
data12se_genus_higher_worms <- data12se_genus_higher_worms_df %>%
  #rename query column
  rename("worms_query" = "query") %>%
  # add in asv associated with each genus - right join because there can be more than on
  right_join(.,
             no_species_id %>%
               select(genus,
                      query) %>%
               drop_na(),
             by = "genus") %>%
  #drop rows of species not found by gbif
  drop_na(worms_query) %>%
  #add indicator for level of query
  mutate(taxo_query_level = "genus") %>%
  #add query source database
  mutate(higher_taxo_source = "worms") %>%
  distinct()

data12se_genus_higher_worms %>% pull(query) %>% unique() %>% length() #2
data12se_genus_higher_worms %>% pull(genus) %>% unique() %>% length() #1

#merge back with rest of table
data12se_taxonomy_resolved <- data12se_genus_higher_worms %>%
  #take the columns that match the other table
  select(c(colnames(data12se_species_gbifid_higher %>%
                    #take out columns that won't match
                      select(-gbif_query,
                           -starts_with("species"),
                           -starts_with("subspecies")
                           )
                  )
           ),
         worms_query) %>%
  full_join(data12se_species_gbifid_higher,
            .)

##clean up table for export
data12se_taxonomy_resolved <- data12se_taxonomy_resolved %>%
  select(-ends_with("_id"))

##export table
write_csv(data12se_taxonomy_resolved,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "taxonomy",
               "clean_data",
               "data12se_taxonomy_r.csv"))

#remove extra information about classification
data12se_taxonomy_r <- data12se_taxonomy_resolved%>%
  select("query",
         "kingdom",
         "phylum",
         "class",
         "order",
         "family",
         "genus",
         "species") %>%
  distinct()

##filter out singleton ASVs
#read in file
data12se_asvmatrix_nc_lor_nfc <- read_csv(here("Processed_data",
                                               "eDNA",
                                               "12s",
                                               "12s_e",
                                               "asv",
                                               "matrix",
                                               "clean_data",
                                               "data12Se_asvmatrix_nc_lor_nfc.csv"))

data12se_taxonomy_r_nc_lor_nfc <- data12se_taxonomy_r %>% 
  filter(query %in% c(data12se_asvmatrix_nc_lor_nfc %>% 
                        pivot_longer(cols = starts_with("ASV"),
                                     names_to = "query",
                                     values_to = "asv_raw_reads") %>%
                        pull(query)))

## filter for animals
data12se_taxonomy_r_nc_lor_nfc_a <- data12se_taxonomy_r_nc_lor_nfc %>% #pull(kingdom) %>% unique() # "Animalia"  "Chromista"
  filter(kingdom == "Animalia")

## filter out terrestrial taxonomies
data12se_taxonomy_r_nc_lor_nfc_a_nt <- data12se_taxonomy_r_nc_lor_nfc_a %>%
  filter(!class %in% c(land_animals_class),
         !family %in% c(land_animals_family))

## export table
write_csv(data12se_taxonomy_r_nc_lor_nfc_a_nt,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "taxonomy",
               "clean_data",
               "data12se_taxonomy_r_nc_lor_nfc_a_nt.csv"))

##check assignments against BC list ----


# 12Su --------------------------------------------------------------------
## read in files ----
data12su_taxonomy <- read.delim(here("Raw_data",
                                   "eDNA",
                                   "12s",
                                   "12s_u",
                                  "taxonomy", #once again doesn't like this folder? 
                                  "MiFish_U_taxonomy_table.12S.NCBI_NT.96sim.txt"),
                                h=TRUE,
                                fill = TRUE)

## clean up column names ----
colnames(data12su_taxonomy) <- data12su_taxonomy %>%
  colnames(.) %>% 
  str_remove_all(string = .,
                 pattern = "X.") #remove X. from column names
data12su_taxonomy <- data12su_taxonomy %>% 
  #clean up column names
  clean_names() 

## clean up data content ----
data12su_taxonomy <- data12su_taxonomy %>%
  # remove anything that contains "unknown"
  mutate(across(.cols = everything(),
                .fns = ~ str_replace_all(string = .,
                                         pattern = "unknown.*",
                                         replacement = NA_character_))
  ) %>%
  # remove "no identification"
  mutate(across(.cols = everything(),
                .fns = ~ str_replace_all(string = .,
                                         pattern = "^no identification$",
                                         replacement = NA_character_))
  ) %>%
  # remove anything that contains "uncultured"
  mutate(across(.cols = everything(),
                .fns = ~ str_replace_all(string = .,
                                         pattern = "uncultured.*",
                                         replacement = NA_character_))
  ) %>%
  # filter out "filtered out" hits
  filter(species != "filtered out")

## clean up taxonomic assignment ----
#strip higher taxonomy and repopulate with correct assignments
# resolve species names
data12su_species_resolved <- tol_resolve(data12su_taxonomy %>%
                                           #remove nas
                                           drop_na(species) %>%
                                           #take species vector
                                           pull(species) %>% #length() - 606 asvs identified to species
                                           #take distinct values
                                           unique(.) #%>% length() - 87 unique species identified
                                         )

data12su_species_resolved %>%
  pull(is_synonym) %>%
  unique() #none are synonyms so use original names

# get gbif ids for the scientific names
data12su_species_gbifid <- get_gbifid(sci = data12su_taxonomy %>%
                                        #remove nas
                                        drop_na(species) %>%
                                        #take species vector
                                        pull(species) %>% #length() - 523 asvs identified to species
                                        #take distinct values
                                        unique(.), #%>% length() - 74 unique species identified
                                      #run interactively - if more than one id is found, ask me to chose - check taxonomy_decisions.txt for decisions made
                                      # input 1 each time it asks
                                      ask = TRUE) #found 74/74 gbifids
#make into a dataframe
data12su_species_gbifid_df <- data12su_species_gbifid %>% 
  as.data.frame(.) %>%
  #add in species info
  mutate(species = data12su_taxonomy %>%
           #remove nas
           drop_na(species) %>%
           #take species vector
           pull(species) %>% 
           #take distinct values
           unique(.)
  )

#get higher taxonomy
data12su_species_gbifid_higher <- classification(sci_id = data12su_species_gbifid_df %>%
                                                   #filter out the ones not found
                                                   filter(match == "found") %>%
                                                   pull(ids) %>%
                                                   unique(), #%>% length(), - 82 ids
                                                 db = 'gbif',
                                                 #give back ID
                                                 return_id = TRUE) %>%
  #bind them together
  cbind(.) %>%
  #remove classifications that didn't reach the species level
  drop_na(species)

data12su_species_gbifid_higher %>% pull(species) %>% unique() %>% length() #66 species

# reformat table
data12su_species_gbifid_higher2 <- data12su_species_gbifid_higher %>% 
  # rename query row
  rename("gbif_query" = "query") %>% 
  #add in asv associated with each species - right join because there can be more than one
  right_join(., 
             data12su_taxonomy %>%
               #take columns of interest
               select(species,
                      query) %>%
               #remove rows containing NA
               drop_na() %>%
               #merge with gbifid
               full_join(.,
                         data12su_species_gbifid_df %>% 
                           #select columns of interest
                           select(species,
                                  ids) %>%
                           #rename column for later merge
                           rename("gbif_query" = "ids")
               ) %>%
               #take columns for merge
               select(gbif_query, 
                      query),
             by = "gbif_query")%>%
  #drop rows of species not found by gbif
  drop_na(species_id) %>%
  #add indicator for level of query
  mutate(taxo_query_level = "species") %>%
  #add query source database
  mutate(higher_taxo_source = "gbif") %>%
  #clean column names
  clean_names()

data12su_species_gbifid_higher2 %>% pull(species) %>% unique() %>% length() #66
data12su_species_gbifid_higher2 %>% pull(query) %>% unique() %>% length() #506

# check what species didn't get an gbif identification
no_species_id_gbif <- data12su_taxonomy %>%
  filter(!query %in% c(data12su_species_gbifid_higher2 %>%
                         pull(query)
                       )
         )

# look at the species against worms 
data12su_species_worms <- get_wormsid(sci_com = no_species_id_gbif %>%
                                        drop_na(species) %>%
                                        pull(species) %>%
                                        unique(.), #%>% length() - 8 unique species
                                      #run in interactive mode
                                      ask = TRUE) #none were found

no_species_id_gbif %>% pull(species) %>% unique() 
#^ these names will be filtered out

# now lets look at genus that haven't been fixed at species level
no_species_id <- data12su_taxonomy %>%
  filter(!query %in% c(data12su_species_gbifid_higher2 %>%
                         pull(query)
                       )
         ) %>%
  #remove species
  select(-species) %>%
  #remove unavailable genus
  drop_na(genus)

no_species_id %>% pull(genus) %>% unique() %>% length() # 4

data12su_genus_resolved <- tol_resolve(no_species_id %>%
                                         #take species vector
                                         pull(genus) %>%
                                         #take distinct values
                                         unique(.))
data12su_genus_resolved %>%
  pull(is_synonym) %>%
  unique() #none are synonyms, so we can use the ones we have (some of the resolved names are weird)

#add in higher taxonomy that has already been found by gbif
data12su_genus_higher <- no_species_id %>%
  #take only genus and ASV id
  select(genus, 
         query) %>%
  #merge with higher taxonomy
  left_join(.,
            data12su_species_gbifid_higher2 %>%
              #remove lower taxo levels than genus
              select(-starts_with("species"),
                     -starts_with("subspecies"),
                     -query,
                     -taxo_query_level,
                     -gbif_query) %>%
              #take distinct rows
              distinct()) %>%
  #remove ones not resolved
  drop_na(genus_id) %>%
  #add indicator of which taxonomic level the higher taxonomy was determined from
  mutate(taxo_query_level = "genus") 

#merge back with other table
data12su_genus_higher <- data12su_genus_higher %>%
  full_join(data12su_species_gbifid_higher2,
            .)

# look at genus that haven't been fixed
no_species_id <- data12su_taxonomy %>%
  filter(!query %in% c(data12su_genus_higher %>%
                         pull(query)
                       )
         ) %>%
  #remove species
  select(-species) %>%
  #remove unavailable genus
  drop_na(genus)

# get species for the genus so can pull higher taxonomy
unresolved_spe_gbif <- downstream(sci_id = no_species_id %>%
                                    pull(genus) %>%
                                    unique(),
                                  db = "gbif",
                                  #only return down to species
                                  downto = "species",
                                  #only give back 1 species for each genus
                                  limit = 1) %>%
  cbind()

# extract dataframe of species
species_4query <- unresolved_spe_gbif %>%
  as.data.frame(.) %>% 
  rownames_to_column(var = "unresolved_genus") %>% 
  rename("species_dfs" = ".") %>%
  filter(lengths(species_dfs) > 0) %>% 
  pull(species_dfs) %>% 
  #combine into one dataframe
  bind_rows() %>%
  #add in unresolved_genus
  mutate(unresolved_genus =  unresolved_spe_gbif %>%
           as.data.frame(.) %>% 
           rownames_to_column(var = "unresolved_genus") %>%
           rename("species_dfs" = ".") %>%
           filter(lengths(species_dfs) > 0) %>%
           pull(unresolved_genus))

#get higher taxonomy
data12su_genus_higher_gbif <- classification(sci_id = species_4query %>%
                                                #take gbifid
                                                pull(key),
                                              db = 'gbif',
                                              #give back ID
                                              return_id = TRUE) %>%
  #bind them together
  cbind(.) %>% 
  #strip info lower than species
  select(-starts_with("species")) %>%
  #take distinct rows
  distinct()

# reformat table
data12su_genus_higher_gbif <- data12su_genus_higher_gbif %>%
  #rename query column
  rename("gbif_query" = "query") %>%
  # add in asv associated with each genus - right join because there can be more than on
  right_join(.,
             no_species_id %>%
               select(genus,
                      query) %>%
               drop_na(),
             by = "genus") %>%
  #drop rows of species not found by gbif
  drop_na(gbif_query) %>%
  #add indicator for level of query
  mutate(taxo_query_level = "genus") %>%
  #add query source database
  mutate(higher_taxo_source = "gbif")

data12su_genus_higher_gbif %>% pull(query) %>% unique() %>% length() #4
data12su_genus_higher_gbif %>% pull(genus) %>% unique() %>% length() #2

#merge back with rest of table
data12su_taxonomy_higher <- data12su_genus_higher_gbif %>%
  full_join(data12su_genus_higher,
            .)

# look at genus that haven't been fixed
no_species_id <- data12su_taxonomy %>%
  filter(!query %in% c(data12su_taxonomy_higher %>%
                         pull(query)
                       )
         ) %>%
  #remove species
  select(-species) %>%
  #remove unavailable genus
  drop_na(genus)

# get species for the genus so can pull higher taxonomy
unresolved_spe_worms <- downstream(sci_id = no_species_id %>%
                                    pull(genus) %>%
                                    unique(),
                                  db = "worms",
                                  #only return down to species
                                  downto = "species",
                                  #only give back 1 species for each genus
                                  limit = 1) %>%
  cbind() #1/1 found

# extract dataframe of species
species_4query <- unresolved_spe_worms %>%
  as.data.frame(.) %>% 
  rownames_to_column(var = "unresolved_genus") %>% 
  rename("species_dfs" = ".") %>%
  filter(lengths(species_dfs) > 0) %>% 
  pull(species_dfs) %>% 
  #combine into one dataframe
  bind_rows() %>%
  #add in unresolved_genus
  mutate(unresolved_genus =  unresolved_spe_gbif %>%
           as.data.frame(.) %>% 
           rownames_to_column(var = "unresolved_genus") %>%
           rename("species_dfs" = ".") %>%
           filter(lengths(species_dfs) > 0) %>%
           pull(unresolved_genus))

#get higher taxonomy
data12su_genus_higher_worms <- classification(sci_id = species_4query %>%
                                               #take first row only
                                               slice_head() %>%
                                               #take gbifid
                                               pull(id),
                                             db = 'worms',
                                             #give back ID
                                             return_id = TRUE) %>%
  #bind them together
  cbind(.) %>% 
  #strip info lower than species
  select(-starts_with("species")) %>%
  #take distinct rows
  distinct()

data12su_genus_higher_worms <- data12su_genus_higher_worms %>%
  #rename query column
  rename("worms_query" = "query") %>%
  # add in asv associated with each genus - right join because there can be more than on
  right_join(.,
             no_species_id %>%
               select(genus,
                      query) %>%
               drop_na(),
             by = "genus") %>%
  #drop rows of species not found by gbif
  drop_na(worms_query) %>%
  #add indicator for level of query
  mutate(taxo_query_level = "genus") %>%
  #add query source database
  mutate(higher_taxo_source = "worms") %>%
  distinct()

data12su_genus_higher_worms %>% pull(query) %>% unique() %>% length() #2
data12su_genus_higher_worms %>% pull(genus) %>% unique() %>% length() #1

#merge back with rest of table
data12su_taxonomy_resolved <- data12su_genus_higher_worms %>%
  #take the columns that match the other table
  select(c(colnames(data12su_taxonomy_higher %>%
                      #take out columns that won't match
                      select(-gbif_query,
                             -starts_with("species"),
                             -starts_with("subspecies")
                             )
                    )
           ),
         worms_query) %>%
  full_join(data12su_taxonomy_higher,
            .)

##clean up table for export
data12su_taxonomy_resolved <- data12su_taxonomy_resolved %>%
  select(-ends_with("_id"))

##export table
write_csv(data12su_taxonomy_resolved,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "taxonomy",
               "clean_data",
               "data12su_taxonomy_r.csv"))

#remove extra information about classification
data12su_taxonomy_r <- data12su_taxonomy_resolved%>%
  select("query",
         "kingdom",
         "phylum",
         "class",
         "order",
         "family",
         "genus",
         "species") %>%
  distinct()

##filter out singleton ASVs
#read in file
data12su_asvmatrix_nc_lor_nfc <- read_csv(here("Processed_data",
                                               "eDNA",
                                               "12s",
                                               "12s_u",
                                               "asv",
                                               "matrix",
                                               "clean_data",
                                               "data12Su_asvmatrix_nc_lor_nfc.csv"))

data12su_taxonomy_r_nc_lor_nfc <- data12su_taxonomy_r %>% 
  filter(query %in% c(data12su_asvmatrix_nc_lor_nfc %>% 
                        pivot_longer(cols = starts_with("ASV"),
                                     names_to = "query",
                                     values_to = "asv_raw_reads") %>%
                        pull(query)))

## filter for animals
data12su_taxonomy_r_nc_lor_nfc_a <- data12su_taxonomy_r_nc_lor_nfc %>% #pull(kingdom) %>% unique() # "Animalia"  "Chromista"
  filter(kingdom == "Animalia")

## filter out terrestrial taxonomies
data12su_taxonomy_r_nc_lor_nfc_a_nt <- data12su_taxonomy_r_nc_lor_nfc_a %>%
  filter(!class %in% c(land_animals_class),
         !family %in% c(land_animals_family))

## export table
write_csv(data12su_taxonomy_r_nc_lor_nfc_a_nt,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "taxonomy",
               "clean_data",
               "data12su_taxonomy_r_nc_lor_nfc_a_nt.csv"))

##check assignments against BC list ----

# trawl -------------------------------------------------------------------
## read in files ----
trawl_catch_sum <- read_csv(here("Processed_data",
                                 "trawl",
                                 "catch_data",
                                 "clean_data",
                                 "trawl_catch_sum.csv"))
## get higher taxonomy ----

## get scientific names
trawl_catch_scinames <- comm2sci(com = c(trawl_catch_sum %>%
                                           drop_na(species) %>%
                                           pull(species) %>% #length() #190 species
                                           unique() # %>% length() #66
                                         ),
                                 db = "worms") 
# reformat
trawl_catch_scinames <- list2DF(trawl_catch_scinames) %>% 
  pivot_longer(cols = everything(),
               names_to = "orig_species",
               values_to = "scientific_name") %>%
  #take distinct rows
  distinct() %>% 
  group_by(orig_species) %>%
  mutate(n = c(1:n())) %>% 
  pivot_wider(names_from = "n",
              values_from = "scientific_name") %>%
  rename("sci_name_1" = "1",
         "sci_name_2" = "2",
         "sci_name_3" = "3",
         "sci_name_4" = "4",
         "sci_name_5" = "5",
         "sci_name_6" = "6",
         "sci_name_7" = "7",
         "sci_name_8" = "8") 

## resolve names
# make empty dataframe
trawl_catch_names_resolved <- tibble()
# loop through each column
for(i in c(colnames(trawl_catch_scinames))){
  ## resolve names
  #make intermediate dataframe
  x <- tol_resolve(names = c(trawl_catch_scinames %>%
                               drop_na({{i}}) %>%
                               pull({{i}}) %>%
                               unique()
                             )
                   ) %>%
    # add in original query (with proper capitilisation)
    mutate(species = trawl_catch_scinames %>%
             drop_na({{i}}) %>%
             pull({{i}}) %>%
             unique())
  #print results
  print(x)
  
  #merge with existing data frame
  trawl_catch_names_resolved <- trawl_catch_names_resolved %>%
    bind_rows(.,
              x)
}

# merge back with sci names
trawl_catch_names_resolved <- trawl_catch_scinames %>% 
  mutate(verbatimIdentification = orig_species) %>%
  pivot_longer(cols = -verbatimIdentification,
               names_to = "name_type",
               values_to = "species") %>%
  full_join(.,
            trawl_catch_names_resolved) %>% 
  drop_na(species)

#export - FOR DISCUSSION
write_csv(trawl_catch_names_resolved,
          here("Processed_data",
               "trawl",
               "catch_data",
               "data_exploration",
               "trawl_catch_names_resolved.csv"))

#remove original names 
sci_names <- subset(trawl_catch_names_resolved, name_type %in% c("sci_name_1", "sci_name_2", "sci_name_3", "sci_name_4", "sci_name_5"))


# get gbif ids for the scientific names
trawl_species_gbifid <- get_gbifid(sci = sci_names %>%
                                     #remove nas
                                     drop_na(species) %>%
                                     #take species vector
                                     pull(species) %>% #length() -
                                     #take distinct values
                                     unique(.), 
                                   ask = TRUE) 

#make into a dataframe
trawl_species_gbifid_df <- trawl_species_gbifid %>% 
  as.data.frame(.) %>%
  #add in species info
  mutate(species = sci_names %>%
           #remove nas
           drop_na(species) %>%
           #take species vector
           pull(species) %>% 
           #take distinct values
           unique(.)
  )

#get higher taxonomy
trawl_species_gbifid_higher <- classification(sci_id = trawl_species_gbifid_df %>%
                                                #filter out the ones not found
                                                filter(match == "found") %>%
                                                pull(ids) %>%
                                                unique(), #%>% 
                                              db = 'gbif',
                                              #give back ID
                                              return_id = TRUE) %>%
  #bind them together
  cbind(.) %>%
  #remove classifications that didn't reach the species level
  drop_na(species)

trawl_species_gbifid_higher %>% pull(species) %>% unique() %>% length() #49 species

#remove non-chordata 
trawl_species_gbifid_higher <- trawl_species_gbifid_higher[trawl_species_gbifid_higher$phylum == 'Chordata',]

#remove duplicates 
trawl_species_gbifid_higher <- trawl_species_gbifid_higher %>% distinct(species, .keep_all = TRUE)

#add common names back into df 
trawl_species <- merge(trawl_species_gbifid_higher, trawl_catch_names_resolved)

#remove duplicates
trawl_species <- new %>% distinct(species, .keep_all = TRUE)

#remove unnecessary columns 
finaltrawltax = subset(trawl_species, select = -c(8:26) )



write_csv(finaltrawltax,
          here("Processed_data",
               "trawl",
               "catch_data",
               "data_exploration",
               "trawl_taxonomy_resolved.csv"))

#compare trawl resolved to known trawl list 
#need to remove species not found in BC 
#need to remove synonyms 
#need to add species that have 'juvenile' or not to species level 

write_csv(cleantrawltax,
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_taxonomy_clean.csv"))

#cleaning trawl catch data (did by hand)

trawl <- read.csv(here::here("Processed_data",
                             "trawl",
                             "catch_data",
                             "clean_data",
                             "trawl_taxonomy_clean.csv"),
                  head=TRUE)

cleaned<- clean_names(trawl$species)

cleaned <- as.data.frame(cleaned) 
colnames(cleaned) <- c('species_clean')

write_csv(cleaned,
          here("Processed_data",
               "trawl",
               "catch_data",
               "clean_data",
               "trawl_taxonomy_clean_species.csv"))


