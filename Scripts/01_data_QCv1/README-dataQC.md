# trawl_eDNA-scripts-dataQCv1

01_initial_cleaning script

  goal: initial cleaning of raw data files and creation of key metatada files 

  inputs: 
  "sequence_table.12S.merged.w_ASV_names.length_var.txt"
  "sequence_table.12S.merged.w_ASV_names.length_var.txt"
  "2018_trawl_eDNA_metadata.csv"
  "trawl_tow_sample_data.csv"
  "Nordic_Pearl_Survey_Trawl_Specimen_Log_11_18_20.xlsx"
  "Trawl_catch_data.csv"

  outputs: 
  "eDNA_metadata.csv" #striped of CO1 information
  "data12Se_asvmatrix_metadata_nc.csv"
  "data12Su_asvmatrix_metadata_nc.csv"
  "trawl_metadata.csv"
  "trawl_catch_sum.csv"
  "trawl_catch.csv"

  issues (needs fixing!): 
  when making trawl metada two issues found (1) biogeo package doesn't seem to work and is needed to create new data file 
  (2) one trawl seems to have a towing time of 0 min (used trawl_metadata.cvs found in original repository)


02_PCRdissimilarity95.R
  goal: remove samples where one or more PCR replicates has a distance to the sample centroid outside a 95% confidence interval

  input: 
  "data12Se_asvmatrix_metadata_nc.csv"
  
  output: 
  no output 

03a_occ_wrangling.R
  goal: occupancy modelling

  input: "data12Se_asvmatrix_metadata_nc.csv"

  outputs: 
  "occProb_royallink_u.csv"
  "ASVs_u.RData"
  "ASVs_u.RData" 

03b_occ_occupancy_model.R
  goal: occupancy modelling, running the model 

  outputs: 
  "occProb_royallink_u.csv"
  "occProb_royallink_u.rds"
  
  notes: 
  jags packages not working 

03c_occ_output_formatting.R
  goal: formatting outputs of occupancy model
  
  input: 
  "occProb_royallink_u.csv"

  outputs: 
  occprob_by_sample.csv
  data12se_asvmatrix_lor_12s_u.csv
  data12su_asvmatrix_nc_lor.csv

04_field_control_read_removal.R
  goal: remove contaminants from sample read numbers according to maximum concentration in negative controls

  inputs: 
  "eDNA_metadata.csv"
  "data12se_asvmatrix_nc_lor.csv"
  "data12su_asvmatrix_nc_lor.csv"

  outputs: 
  "data12Se_asvmatrix_nc_lor_nfc.csv""
  "data12Su_asvmatrix_nc_lor_nfc.csv"
  "data12Se_asv_taxonomy_long_nc_lor_nfc.csv"

05_LTCassignment - modified version of Ben's code 
  goal: assign lowest common taxon to groups 

  inputs: 
  "data12Se_asvmatrix_nc_lor_nfc.csv" #read count (not index) 
  "eDNA_metadata.csv"
  "MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.LCA_ONLY.txt"
  "MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.txt"
  "MiFish_E_12S_ASV_sequences.length_var.blast.out"
  "data12Su_asvmatrix_nc_lor_nfc.csv" #read count (not index)
  "MiFish_U_taxonomy_table.12S.NCBI_NT.96sim.LCA_ONLY.txt"
  "MiFish_U_taxonomy_table.12S.NCBI_NT.96sim.txt"
  "MiFish_E_12S_ASV_sequences.length_var.blast.out"

  outputs: 
  "12setaxonomy.csv" -- later edited for species in/out of range to read in "12setaxonomy2.csv"
  "12sutaxonomy.csv" -- later edited for species in/out of range to read in "12sutaxonomy2.csv"
  "taxonomy_groups_12s_eDNA.csv"
  "ASV_taxonomy_12seDNA.csv"
  "LCT_by_site12se.csv"
  "top10_gbifid_higher.csv" - for 12se and 12su 
  "taxonomy_groups_12u_eDNA.csv"
  "ASV_taxonomy_12suDNA.csv"
  "LCT_by_site12su.csv"
  
  notes: 
  - code on line 298 shows issue + 611

06_eDNA_index.R
  goal: make ASV matrix with eDNA reads 
  for each sample + query we have eDNA index 
  
  
  inputs:
  edna_index.R
  data12Se_asvmatrix_nc_lor_nfc.csv (input into LCT)
  ASV_taxonomy_12seDNA.csv (output of LCT)
  ASV_taxonomy_12suDNA.csv (output of LCT)

  outputs:
  "data12se_asv_index.csv"
  "data12se_taxonomy_index.csv"
  "data12su_asv_index.csv"
  "data12su_taxonomy_index.csv"
  

07_assign_taxonomy_trawl.R
  goal: assigns cleaned taxonomic name to trawl species through curated key 

  inputs: 
  "trawl_catch_sum.csv"
  "trawl_catch.csv"
  "trawl_taxonomy_clean.csv" (curated key)
  

  outputs: 
  "fix_taxonomy 0-3_" interim dataframes that are manually edited to fix names 
  "trawl_sum_clean.csv"
  "trawl_catch_clean"

08_datasetcuration.R 
  goal: makes datasets for analysis 
        removes invalid sets (more than 50m) 
        merges all eDNA data (12su/12se)
        aggregates to set number 
        takes sum of index per species per set number
        takes sum of weight per species per set number

  inputs: 
   "data12se_taxonomy_index.csv"
  "data12su_taxonomy_index.csv"
  "trawl_metadata.csv"
  "data12Su_asvmatrix_metadata_nc.csv"
  "data12Se_asvmatrix_metadata_nc.csv"
  "trawl_sum_clean.csv" #output of assign tax. trawl 

  outputs: 
  "eDNAfulldataset.csv" full eDNA dataset w/ species per set w read count and metadata
  "fulldatasettrawl.csv" full trawl dataset
  "trawl_catch_weight" 
  "eDNA_allsets_" includes sets >50m
  "trawl_allsets_" includes sets >50m
  "trawlweight_allsets_" includes sets >50m


09_detect_methods.R
  goal: make a dataset for analysis on diversity by adding detection 
 		method at gamma, beta and alpha levels 
  
  inputs: 
  	"eDNAfulldataset.csv"
  	"trawl_catch_weight.csv"
  	trawl_metadata.csv
  	"trawl_allsets_" includes sets >50m
    "trawlweight_allsets_" includes sets >50m
  
  outputs: 
  
  	"detections.csv" detection method for each species
  	"detection_env.csv" detection dataset merged w. metadata
	"detections_all.csv_" detectetions for all sets (including >50m)
	"detectiona_all_env.csv"

