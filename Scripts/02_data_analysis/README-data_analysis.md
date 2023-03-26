README - data_analysis

  
FOLDER: Diversity 
This folder contains scripts for comparing community assemblages


gamma_diversity.R
  goal: look at species detected by trawl/eDNA across all sites, creates quantitative + 
  qualitative Euler plot 
  
  inputs: 
  "detections.csv"
  "detections_all.csv"
  
  output: 
  gamma_diversity.png
  gamma_diversity_allsets.png
  
beta_diversity.R
  goal: look at species detected by trawl/eDNA across North and South sites, creates 
  quantitative Euler plots (2 for N, 2 for S)
  
  inputs: 
  "detections.csv
  "detections_all_.csv"
  "trawl_metadata.csv"

  outputs: 
  south_north_euler.png N/S euler plots together  
  south_north_euler_allsets.png" 
  #also produces euler and venn diagrams from an earlier code as separate N/S
  

set_diversity.R 
  goal: look at species detected by trawl/eDNA across EACH set,  creates quantitative + 
  qualitative Euler plots (2 per site)
  
  inputs: 
	"detections.csv"
	"detections_all_.csv"

  
  outputs:
  alpha_eulerr_all.png"
  alpha_eulerr.png"
  

Jaccards_analyses.R
  goal: calculate Jaccard indices + produce graph
  
  inputs: 
  "detections.csv"
  "detections_all.csv"
  
  outputs: 
  "diversity_indices.csv"
  "diversity_indices_all.csv"
  "jaccards_set.png"
  jaccards_set_all.png"
  "jaccards_region.png"
  jaccards_region_all.png"

  
Jaccard_table.R

	input: 
	"diversity_indices.csv"
	"diversity_indices_all.csv"
	
	output: 
	nestednessturnovercomponents.png"
	nestednessturnovercomponents_all.png
	tables of dissimilarities 
	
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
   length_bymethod.png"

   
habitat.R
	goal: visualize the habitats patterns of detections using alluvia plots 

	
	inputs: 
	"detections_all.csv"
	"traitdatabase.csv"
	
	outputs: 
	habitat_alluvia.png"
	
	note: old code in this script uses chord diagrams to show habitat relationships
	
the following scripts are no longer used in the analysis: 

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
	"species_biomass_sum.csv"
	"species_biomass_all.csv"
	eulerbiomass.png"
	eulerbiomass_all.png"
	

FOLDER: meta

meta.R
	goal: create graph of sampling depths between eDNA + trawl 
	
	inputs: 
	"trawl_metadata.csv"
	"eDNA_metadata.csv"
	
	outputs: 
	"samplingdepths.png"

  