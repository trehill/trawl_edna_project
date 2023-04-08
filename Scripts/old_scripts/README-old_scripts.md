READ ME OLD SCRIPTS 

harmonize_taxonomy ###NOT USING THIS ANYMORE (OR NEED TO ADAPT?)
  goal: taxonomic harmonization between trawl and 12se and 12su
  
  inputs: 
  "trawl_taxonomy_clean.csv" -trawl higher taxonomy species list, manually edited as a key  
  "data12se_taxonomy_r_nc_lor_nfc_a_nt.csv" - 12se higher taxonomy species list
  "data12su_taxonomy_r_nc_lor_nfc_a_nt.csv" - 12su higher taxonomy species list 

  outputs: 
  "12se_12su_species.csv" - species that are in both 12se and 12su datasets  
  "trawl_12se_species.csv" - species that are in both trawl and 12se 
  "speciesonly12su" -12su species only 
  "speciesonly12se" -12se species only 
  "onlytrawlspecies" - trawl species only
  
taxonomy_cleaning #redundant now
  goal: taxonomic harmonization across 12s and trawl species detected 

  inputs: 
  MiFish_E_taxonomy_table.12S.NCBI_NT.96sim.txt
  "data12Se_asvmatrix_nc_lor_nfc.csv" 
  "data12Su_asvmatrix_nc_lor_nfc.csv"
  "trawl_catch_sum.csv"

  outputs: 
  "data12se_taxonomy_r.csv" - higher taxonomy for 12se species 
  "data12se_taxonomy_r_nc_lor_nfc_a_nt.csv" -ASV query assigned to higher taxonomy (cleaned) 
  "data12su_taxonomy_r.csv - higher taxonomy for 12su species 
  "data12Su_asvmatrix_nc_lor_nfc.csv" - removed field reads
  "data12su_taxonomy_r_nc_lor_nfc_a_nt.csv" 
  "trawl_catch_names_resolved.csv" - exploratory data for trawl name taxonomic resolution 
  "trawl_taxonomy_resolved.csv" - exploratory data higher taxonomy, only chordata 

  issues: 
  - need to check 12se/12su against BC species lists 
  - need to check trawl taxonomy if it makes sense against raw data 
  - output of 'trawl_taxonomy_resolved.csv" currently contains duplicates, tax. synonyms, no 'juvenile' input or non-species level   inputs
  
07_assign_taxonomy_eDNA.R #i think this is useless now
  goal: assigns ASV to sample and LCT (this is output of eDNA index)

  inputs: 
  data12Se_asvmatrix_nc_lor_nfc.csv #same input to Ben's code 
  ASV_taxonomy_12seDNA.csv #output of Ben's code 
  data12Su_asvmatrix_nc_lor_nfc.csv #same input to Ben's code
  ASV_taxonomy_12suDNA.csv #output of Ben's code 

  outputs: 
  ASVtaxonomybysample12se.csv #samples assigned to ASV and taxonomy 
  ASVtaxonomybysample12su.csv #samples assigned to ASV and taxonomy 


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
	"detections_all.csv_" detectetions for all sets (including >50m)


depth.R
	goal: investigate relationships between difference in depth + diversity indices 

	inputs: 
	"detections.csv"
	"traitdatabase.csv"
	"trawl_catch_clean.csv"
	"trawl_metadata.csv"
	"eDNA_metadata.csv"
	"diversity_indices_all.csv"
	
	outputs: 
	depth_jac.png
	depth_nest.png
	depth_turn.png
	"depth.csv"
	depth_shared.png
	nest_turn.png
	
length_stat_analysis.R
	goal: use statistics to detect differences in length distributions 
	
	inputs: 
	"length_2.csv
	"length.csv
	
	outputs: 
	indvsmax.png
	onlyeDNAtrawl.png
	alltrawledna.png
	
