# trawl_eDNA-
SCRIPTS 
FOLDER: data_analysis 
contains scripts for exploratory analysis with euler plots 

TRtrait_collection.R 

inputs: 

outputs: 

notes: 
- subset down to species level, unsure how to deal with LCT 

TRExploreEuler.R 

inputs: 
outputs: 

notes: 
- add removing >50m samples to dataQC scripts instead of doing this at the beginning of the code 


FOLDER: functions
contains functions for data_QCv1 scripts 

eDNA_index.R: computes eDNA index on taxonomy by sample matrix from raw read numbers

FOLDER: occupancy_modelling 
contains .csv files from outputs of data_QCv1 scripts using occupancy modelling 


FOLDER: occupancy_modelling 
Contains files and datasets used as inputs/outputs in the occupancy modelling 

FILES 
"taxonomy_taxonomy_clean" -taxonomy cleaned by hand, took list of raw data species and "trawl_taxonomy_resolved", removed duplicates
and species that were synonym and do not have a distribution in BC, added species that were not included in "trawl_taxonomy_resolved" due to 'juvenile' input or not to species level 


