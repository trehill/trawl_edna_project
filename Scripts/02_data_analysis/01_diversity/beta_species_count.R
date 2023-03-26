#getting some numbers

library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(dplyr)

data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

meta <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "metadata", 
                            "clean_data",
                            "trawl_metadata.csv"),
                 head=TRUE)

data2 <- merge(data, meta, by=c('set_number'))

northern <-  data2[data2$leg == 'northern',]
southern <-  data2[data2$leg == 'southern',]

#what species were detected in each region 
unique(northern$LCT) #20 species total 

unique(southern$LCT) #36 species total 

#how many species in both at each region 
n_beta <-  northern[northern$beta_detection_method == 'both eDNA/trawl',]
s_beta <-  southern[southern$beta_detection_method  == 'both eDNA/trawl',]

unique(n_beta$LCT) #8 species total 

unique(s_beta$LCT) #12 species total 

#how many species only in eDNA in each region 

n_beta <-  northern[northern$beta_detection_method == 'only eDNA',]
s_beta <-  southern[southern$beta_detection_method  == 'only eDNA',]

unique(n_beta$LCT) #5 species total 

unique(s_beta$LCT) #18 species total 

#how many species only in trawl at each region 
n_beta <-  northern[northern$beta_detection_method == 'only trawl',]
s_beta <-  southern[southern$beta_detection_method  == 'only trawl',]

unique(n_beta$LCT) #7 species total 

unique(s_beta$LCT) #6 species total 
