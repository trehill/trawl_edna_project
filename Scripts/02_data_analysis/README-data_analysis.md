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
  

Jaccards_analyses.R
  goal: calculate Jaccard indices + produce graph
  
  inputs: 
  "detections.csv"
  
  outputs: 
  "diversity_indices.csv"
  "jaccards_set.png"
  "jaccards_region.png"
  nestednessturnovercomponents.png"
  
species_count.R
goal: counts occurence of species in whole dataset (eventually will be phylopic code)
  
  inputs: 
   "detections.csv"
   
   outputs: 
   "species_count.csv" #count of observations per species, used to make 'picture' Venn
   
FOLDER: Traits 

trait_collection.R
  goal: use rFishBase package to extract traits for each species detected between both methods 
  
  **currently this script is not working
  the rfishbase package functions take FOREVER to run, ends up timing out each time 
  created a trait database by hand for each species 

traits.R 
  goal: exploratory analysis of traits 
  
  inputs: 
  "detections.csv"
  "traitdatabase.csv" curated trait database by hand by searching FishBase (eventually could write                       script "trait_collection" to do this with code )
   "method_key.csv"
   "length_conversions.csv"
   
   outputs: 
   - histogram of individual species length 
   - bar graph for preffered environment within water column 
   - density plot across species lengths (both + individual methods)

   
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
	
lengthconversions.R
	goal: convert fork length to total length for species caught in trawl 
	
	inputs:
	"trawl_catch_clean.csv"
	
	outputs: 
	"length_conversions.csv" 
	
habitat.R
	goal: data visualization of habitat types for species detected by trawl, eDNA and both
	
	inputs: 
	
	

FOLDER: Index 

index.R 
	goal: exploring eDNA read index over biomass 
	
	inputs: 
	"detections.csv
	"trawl_metadata.csv
	
	outputs: 
	biomass_index.png
	biomass_index_log.png

biomass.R 

	goal: compare biomass relationship between trawl + eDNA
	
	inputs: 
	"detections.csv"
	"trawl_metadata.csv"
	
	outputs: 
	biomass_box.png"
	
mean_biomass.R
	goal: calculates the mean biomass per and creates euler plot for data visualization
	
	inputs 
	"biomass.csv"
	
	outputs
	"species_biomass.csv"
	eulerbiomass.png"
	

FOLDER: meta

meta.R
	goal: create graph of sampling depths between eDNA + trawl 
	
	inputs: 
	"trawl_metadata.csv"
	"eDNA_metadata.csv"
	
	outputs: 
	"samplingdepths.png"

  