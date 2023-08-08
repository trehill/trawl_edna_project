#eDNA index values (from reads)
### Goal = convert read numbers to eDNA index and output final quality controlled tables
### Authors = Anya Mueller + Tessa Rehill 


# Set up ------------------------------------------------------------------
## install packages
# install the following packages if needed
#install.packages("tidyverse")
#install.packages("here")

## read in packages
library(tidyverse)
library(here)

## read in functions
source(here("Scripts",
            "functions",
            "edna_index.R"))


# 12se --------------------------------------------------------------------
## read in files
data12se_asvmatrix_nc_lor_nfc <- read_csv(here("Processed_data",
                                        "eDNA",
                                        "12s",
                                        "12s_e",
                                        "asv",
                                        "matrix",
                                        "clean_data",
                                        "data12Se_asvmatrix_nc_lor.csv")) %>%
  rename("original_sample_name" = "...1")

TRdata12se_taxonomy <- read_csv(here("Processed_data", 
                                                     "eDNA",
                                                     "12s",
                                                     "12s_e",
                                                     "LCTassignment",
                                                     "ASV_taxonomy_12seDNA.csv")) 

#change the first column "ASV" to query (keep consistent)
names(TRdata12se_taxonomy)[names(TRdata12se_taxonomy) == 'ASV'] <- 'query'

## take eDNA index for asv  
data12se_asvmatrix_nc_lor_nfc_ei <- data12se_asvmatrix_nc_lor_nfc %>% 
  edna_index(df = .,
             row_var = "original_sample_name")

## reformat table, add in taxonomic info 
data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- data12se_asvmatrix_nc_lor_nfc_ei %>% 
  #pivot dataframe
  pivot_longer(cols = starts_with("ASV"),
               names_to = "query",
               values_to = "asv_read_index") %>%
  #merge with raw read numbers
  full_join(.,
            data12se_asvmatrix_nc_lor_nfc %>%
              pivot_longer(cols = starts_with("ASV"),
                           names_to = "query",
                           values_to = "asv_read_raw")) %>%
  #merge with taxonomies
  right_join(.,
             TRdata12se_taxonomy) #only retain ASVs with taxonomic assignments


#add column of raw read numbers for each taxonomic level
for(var in c("class","order","family","genus","species")){
  
  #make new column name
  varname <- paste0(var, "_read_raw")
  
  #make raw read number columns
  x <- data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
    #group by sample and taxonomy
    group_by(original_sample_name,
             .data[[var]]) %>%
    summarise(!!varname := sum(asv_read_raw)) %>%
    ungroup()
  
  #merge back
  data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- left_join(data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt,
                                                  x)
}

## calculate taxonomic read index - this needs to be done at each taxonomy level
for(var in c("class","order","family","genus","species")){
  
  #make taxonomy matrix
  x <- data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
    select(original_sample_name,
           starts_with(var)) %>% 
    drop_na(var) %>%
    #take distinct rows
    distinct() %>%
    #pivot into matrix form
    pivot_wider(names_from = var,
                values_from = paste0(var, "_read_raw")) 
  
  #take edna index
  x <- x %>% 
    edna_index(df = .,
               row_var = "original_sample_name")
  
  #reformat x for merge
  x <- x %>% 
    pivot_longer(cols = c(-original_sample_name),
                 names_to = var,
                 values_to = paste0(var, "_read_index"))
  
  #merge back
  data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- left_join(data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt,
                                                  x)
}

#add presence absence columns
for(var in c("asv","class","order","family","genus","species")){
  
  #make name for new variable
  newvarname <- paste0(var, "_pa")
  #make name for if_else condition
  existingvarname <- paste0(var, "_read_index")
  
  #add new variable 
  x <- data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
    mutate(!!newvarname := if_else(condition = c(data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt[[existingvarname]] == 0),
                                   true = 0,
                                   false = 1))
  #merge back
  data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- left_join(data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt,
                                                  x)
}

## subset to taxonomy and asv matrices
data12se_asv_long_nc_lor_nfc_ei_r_a_nt <- data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
#select asv data
select(original_sample_name,
       query,
       starts_with("asv")) %>%
  #take distinct rows
  distinct()

data12se_taxonomy_long_nc_lor_nfc_ei_r_a_nt <- data12se_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
  #select asv data
  select(original_sample_name,
         query,
         starts_with("kingdom"),
         starts_with("phylum"),
         starts_with("class"),
         starts_with("order"),
         starts_with("family"),
         starts_with("genus"),
         starts_with("species")) %>%
  #take distinct rows
  distinct()


data12se_taxonomy_long_nc_lor_nfc_ei_r_a_nt <- data12se_taxonomy_long_nc_lor_nfc_ei_r_a_nt %>% 
  rename(LCT = species)


## export tables
write_csv(data12se_asv_long_nc_lor_nfc_ei_r_a_nt,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
                "eDNAindex",
               "data12se_asv_index.csv"))

write_csv(data12se_taxonomy_long_nc_lor_nfc_ei_r_a_nt,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_e",
               "eDNAindex",
               "data12se_taxonomy_index.csv"))


# 12Su --------------------------------------------------------------------
## read in files
data12su_asvmatrix_nc_lor_nfc <- read_csv(here("Processed_data",
                                               "eDNA",
                                               "12s",
                                               "12s_u",
                                               "asv",
                                               "matrix",
                                               "clean_data",
                                               "data12su_asvmatrix_nc_lor.csv")) %>%
  rename("original_sample_name" = "...1")
  

TRdata12su_taxonomy <- read_csv(here("Processed_data", 
                                     "eDNA",
                                     "12s",
                                     "12s_u",
                                     "LCTassignment",
                                     "ASV_taxonomy_12suDNA.csv"))

#change the first column "ASV" to query (keep consistent)
names(TRdata12su_taxonomy)[names(TRdata12su_taxonomy) == 'ASV'] <- 'query'

## take eDNA index for asv 
data12su_asvmatrix_nc_lor_nfc_ei <- data12su_asvmatrix_nc_lor_nfc %>% 
  edna_index(df = .,
             row_var = "original_sample_name")

## reformat table, add in taxonomic info
data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- data12su_asvmatrix_nc_lor_nfc_ei %>% 
  #pivot dataframe
  pivot_longer(cols = starts_with("ASV"),
               names_to = "query",
               values_to = "asv_read_index") %>%
  #merge with raw read numbers
  full_join(.,
            data12su_asvmatrix_nc_lor_nfc %>%
              pivot_longer(cols = starts_with("ASV"),
                           names_to = "query",
                           values_to = "asv_read_raw")) %>%
  #merge with taxonomies
  right_join(.,
             TRdata12su_taxonomy) #only retain ASVs with taxonomic assignments

#add column of raw read numbers for each taxonomic level
for(var in c("order","family","genus","species")){
  
  #make new column name
  varname <- paste0(var, "_read_raw")
  
  #make raw read number columns
  x <- data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
    #group by sample and taxonomy
    group_by(original_sample_name,
             .data[[var]]) %>%
    summarise(!!varname := sum(asv_read_raw)) %>%
    ungroup()
  
  #merge back
  data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- left_join(data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt,
                                                          x)
}


## calculate taxonomic read index - this needs to be done at each taxonomy level
for(var in c("order","family","genus","species")){
  
  #make taxonomy matrix
  x <- data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
    select(original_sample_name,
           starts_with(var)) %>% 
    drop_na(var) %>%
    #take distinct rows
    distinct() %>%
    #pivot into matrix form
    pivot_wider(names_from = var,
                values_from = paste0(var, "_read_raw")) 
  
  #take edna index
  x <- x %>% 
    edna_index(df = .,
               row_var = "original_sample_name")
  
  #reformat x for merge
  x <- x %>% 
    pivot_longer(cols = c(-original_sample_name),
                 names_to = var,
                 values_to = paste0(var, "_read_index"))
  
  #merge back
  data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- left_join(data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt,
                                                          x)
}

#add presence absence columns
for(var in c("asv","order","family","genus","species")){
  
  #make name for new variable
  newvarname <- paste0(var, "_pa")
  #make name for if_else condition
  existingvarname <- paste0(var, "_read_index")
  
  #add new variable 
  x <- data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
    mutate(!!newvarname := if_else(condition = c(data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt[[existingvarname]] == 0),
                                   true = 0,
                                   false = 1))
  #merge back
  data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt <- left_join(data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt,
                                                          x)
}

## subset to taxonomy and asv matrices
data12su_asv_long_nc_lor_nfc_ei_r_a_nt <- data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
  #select asv data
  select(original_sample_name,
         query,
         starts_with("asv")) %>%
  #take distinct rows
  distinct()

data12su_taxonomy_long_nc_lor_nfc_ei_r_a_nt <- data12su_asv_taxonomy_nc_lor_nfc_ei_r_a_nt %>%
  #select asv data
  select(original_sample_name,
         query,
         starts_with("kingdom"),
         starts_with("phylum"),
         starts_with("class"),
         starts_with("order"),
         starts_with("family"),
         starts_with("genus"),
         starts_with("species")) %>%
  #take distinct rows
  distinct()


data12su_taxonomy_long_nc_lor_nfc_ei_r_a_nt <- data12su_taxonomy_long_nc_lor_nfc_ei_r_a_nt %>% 
  rename(LCT = species)

## export tables
write_csv(data12su_asv_long_nc_lor_nfc_ei_r_a_nt,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "eDNAindex",
               "data12su_asv_index.csv"))

write_csv(data12su_taxonomy_long_nc_lor_nfc_ei_r_a_nt,
          here("Processed_data",
               "eDNA",
               "12s",
               "12s_u",
               "eDNAindex",
               "data12su_taxonomy_index.csv"))

