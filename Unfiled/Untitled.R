#goal: find matches between eDNA full datasets species and the trawl dataset 

#Set-Up
library(here)
library(dplyr)


## read in data files 
## read in trawl species 

trawl <- read.csv(here::here("Processed_data",
                                   "trawl",
                                   "fulldatasettrawl.csv"),
                        head=TRUE)

eDNA <- read.csv(here::here("Processed_data",
                            "eDNA",
                            "ednafulldataset.csv"),
                 head=TRUE)


#look into unique species across both dataset 
#first, we'll look at trawl species 

unique(trawl$species) #let's look into the NA values, these are species that might need an LCT 
unique(eDNA$LCT)

#NA 1: 	Corphaenidoes
#NA 2: Unidentified Rockfish 

trawl$species = trawl$LCT #create new column (LCT) 