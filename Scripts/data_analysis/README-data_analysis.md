README - data_analysis

  
FOLDER: Diversity 
This folder contains scripts for comparing community assemblages

Qualitative Analysis: 

all_diversity.R
  goal: look at species detected by trawl/eDNA across all sites, creates quantitative + 
  qualitative Euler plot 
  
  inputs: 
  "fulldatasettrawlmin.csv"
  "eDNAfulldatasetmin.csv"
  
  output: 
  "method_key.csv" - method key only works at the gamma level 
  quantitative euler plot (number)
  qualitative euler plot (species name)
  
beta_diversity.R 
  goal: look at species detected by trawl/eDNA across EACH site, creates quantitative + 
  qualitative Euler plots (2 per site)
  
  inputs: 
  "fulldatasettrawlmin.csv"
  "eDNAfulldatasetmin.csv"
  
  outputs:
  1 qualitative eulerr plot per site 
  1 quantitative euler plot per site 
  
gamma_diversity.R
  goal: look at species detected by trawl/eDNA across North and South sites, creates 
  quantitative Euler plots (2 for N, 2 for S)
  
  inputs: 
  "fulldatasettrawlmin.csv"
  "eDNAfulldatasetmin.csv"
  
  outputs: 
  1 qualitative eulerr plot per region 
  1 quantitative euler plot per region
  
Quantitative Analysis 

beta_div.R
  goal: makes datasets for quantitative analysis 
  
  inputs: "eDNAfulldataset.csv"
  outputs: "beta_div.csv"
  
betadiver_analysis.R
  goal: calculate Jaccard indices 
  
  inputs: "beta_div.csv"
  outputs: betadiff.png
  
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
   
   
   

  