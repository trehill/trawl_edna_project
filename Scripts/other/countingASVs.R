#Counting ASVs


#SCRIPT 1 - initial cleaning
## read in files
#data12Se_asvmatrix <- read.table(here::here("Raw_data",
#                                            "eDNA",
#                                            "12s",
#                                           "12s_e",
#                                            "asv",
#                                            "matrix",
#                                            "sequence_table.12S.merged.w_ASV_names.length_var.txt"),
#                                 h=TRUE)

#data12Su_asvmatrix <- read.table(here::here("Raw_data",
#                                            "eDNA",
#                                            "12s",
 #                                           "12s_u",
 #                                           "asv",
 #                                           "matrix",
 #                                           "sequence_table.12S.merged.w_ASV_names.length_var.txt"),
 #                                h=TRUE)

#input
#count number of rows 
ncol(data12Se_asvmatrix) #764
ncol(data12Su_asvmatrix) #647

#output
#det <- read.csv(here::here("Processed_data", 
#                           "data12Se_asvmatrix_metadata_nc.csv"), 
#                head=TRUE)        



#dat <- read.csv(here::here("Processed_data", 
#                          "data12Su_asvmatrix_metadata_nc.csv"),  
#               head=TRUE)        

#select only rows with ASVs
new_det <- det[, 22:ncol(det)]
new_dat <- dat[, 22:ncol(dat)]

ncol(new_det) #763
ncol(new_dat) #646

#SCRIPT 2 - no output??

#SCRIPT 3 - occupancy modelling

#input as above 
#output 
occ1 <- read_csv(here::here("Scripts",
                                          "occupancy_modelling",
                                          "royle_link",
                                          "scratch",
                                          "data12se_asvmatrix_lor_12s_u.csv"))

ncol(occ1) #49

occ2 <- read.csv(here::here("Processed_data", 
                           "eDNA",
                           "12s",
                           "12s_u",
                           "asv",
                           "matrix",
                           "clean_data",
                           "data12su_asvmatrix_nc_lor.csv"), #file that contains the incidence of each LCT 
                head=TRUE)        

ncol(occ2) #49

#SCRIPT  4: field control removals 

ASVbysite <- read.csv(here::here("Processed_data", 
                                 "eDNA",
                                 "12s",
                                 "12s_e",
                                 "asv",
                                 "matrix",
                                 "clean_data",
                                 "data12Se_asvmatrix_nc_lor_nfc.csv"),
                 
                           head=TRUE)
ncol(ASVbysite) #85

ASVbysite <- read.csv(here::here("Processed_data", 
                                 "eDNA",
                                 "12s",
                                 "12s_u",
                                 "asv",
                                 "matrix",
                                 "clean_data",
                                 "data12Su_asvmatrix_nc_lor_nfc.csv"),
                      head=TRUE)

ncol(ASVbysite) #47


#SCRIPT 5: LTC Assignment

LCA_method <- read.csv(here::here("Raw_data", 
                                  "eDNA",
                                  "12s",
                                  "12s_e",
                                  "MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.LCA_ONLY.txt"),
                       head=TRUE)
nrow(LCA_method)

best_hit <-  read.csv(here::here("Raw_data", 
                                 "eDNA",
                                 "12s",
                                 "12s_e",
                                 "MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.txt"),
                      head=TRUE)
nrow(best_hit)


LCA_method <- read.csv(here::here("Raw_data", 
                                  "eDNA",
                                  "12s",
                                  "12s_u",
                                  "MiFish_U_taxonomy_table.12S.NCBI_NT.96sim.LCA_ONLY.txt"),
                       head=TRUE)
nrow(LCA_method)

best_hit <-  read.csv(here::here("Raw_data", 
                                 "eDNA",
                                 "12s",
                                 "12s_u",
                                 "MiFish_U_taxonomy_table.12S.NCBI_NT.96sim.txt"),
                      head=TRUE)

nrow(best_hit)

#SCRIPT 6: eDNA

data12se_taxonomy <- read_csv(here("Processed_data", 
                                   "eDNA",
                                   "12s",
                                   "12s_e",
                                   "LCTassignment",
                                   "ASV_taxonomy_12seDNA.csv")) 

nrow(data12se_taxonomy) #55

data12su_taxonomy <- read_csv(here("Processed_data", 
                                     "eDNA",
                                     "12s",
                                     "12s_u",
                                     "LCTassignment",
                                     "ASV_taxonomy_12suDNA.csv"))

nrow(data12su_taxonomy)  #36
