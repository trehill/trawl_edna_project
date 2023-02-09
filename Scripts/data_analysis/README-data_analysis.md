README - data_analysis

  
FOLDER: Diversity 
This folder contains scripts for comparing community assemblages


gamma_diversity.R
  goal: look at species detected by trawl/eDNA across all sites, creates quantitative + 
  qualitative Euler plot 
  
  inputs: 
  "detections.csv"
  "eDNAfulldataset.csv"
  "fulldatasettrawl.csv"
  
  output: 
  gamma_diversity.png
  
set_diversity.R 
  goal: look at species detected by trawl/eDNA across EACH set,  creates quantitative + 
  qualitative Euler plots (2 per site)
  
  inputs: 
	"detections.csv"
	"eDNAfulldataset.csv"
	"fulldatasettrawl.csv"
  
  outputs:
  produces euler plot of all sets together
  1 qualitative eulerr plot per site 
  1 quantitative euler plot per site 
  
beta_diversity.R
  goal: look at species detected by trawl/eDNA across North and South sites, creates 
  quantitative Euler plots (2 for N, 2 for S)
  
  inputs: 
  "detections.csv"
  "trawl_metadata.csv"
  "eDNAfulldataset.csv"
"fulldatasettrawl.csv"
  
  outputs: 
  south_north_euler.png N/S euler plots together   
  #also produces euler and venn diagrams from an earlier code as separate N/S
  

Jaccards_analyses.R
  goal: calculate Jaccard indices + produce graph
  
  inputs: "detections.csv"
  outputs: 
  "diversity_indices.csv"
  "jaccards_set.png"
  "jaccards_region.png"
  
species_count.R
goal: counts occurence of species in whole dataset (eventually will be phylopic code)_
  
  inputs: 
   "detections.csv"
   
   outputs: 
   "species_count.csv"
   
FOLDER: Traits 

trait_collection.R
  goal: use rFishBase package to extract traits for each species detected between both methods 
  
  **currently this script is not working
  the rfishbase package functions take FOREVER to run, ends up timing out each time 

traits.R 
  goal: exploratory analysis of traits 
  
  inputs: 
  "fulldatasettrawlmin.csv"
  "eDNAfulldatasetmin.csv"
  "traitdatabase.csv" curated trait database by hand by searching FishBase (eventually could write                       script "trait_collection" to do this with code )
   "method_key.csv"
   "trawl_catch.csv"
   
   outputs: 
   - histogram of individual species length 
   - bar graph for preffered environment within water column 
   - density plot across species lengths (both + individual methods)
   "speciestraits.csv"/"speciestraits2.csv" removing "both" by hand (due to time crunch)
   - density plot of depth range across methods 
   
   notes: 
   - need to fix these plots... hist are showing 'zero' values 
   - need to fix removing "both" by hand 
   
   
   

  