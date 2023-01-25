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

