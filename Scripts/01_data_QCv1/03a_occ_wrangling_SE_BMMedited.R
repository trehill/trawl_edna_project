#03a_occ_wrangling.R
#install.packages("jagsUI")
##install.packages("dyplr")

library(tidyverse)
library(jagsUI) #had to manually select packages 
library(here)
library(dplyr)


dat <- read.csv("./Processed_data/eDNA/12s/12s_e/asv/matrix/clean_data/data12Se_asvmatrix_metadata_nc.csv")


spec <- dat[c(22:784)]                               #select Asv Matrix
spec01 <- as.data.frame(ifelse(spec == 0, 0, 1))     #convert to binary
ASVcount <- as.data.frame(colSums(spec01))           #find number of observations of each ASV
ASVcountNo0s <- filter(ASVcount, ASVcount[1] != 0)   #count of detections by ASV
ASV0count <- filter(ASVcount, ASVcount[1] == 0)      #find ASVs with 0 observations
ASVswith0count <- rownames(ASV0count)
ASV1count <- filter(ASVcount, ASVcount[1] == 1)      #find ASVs with 1 observation (one PCR detection in dataset)

spec[,c(ASVswith0count)] <- NULL                     #remove ASVs with 0 observations from ASV matrix
spec01[,c(ASVswith0count)] <- NULL                   #remove ASVs with 0 observations from ASV matrix

ASV_by_sample <- cbind(dat$sample_name, spec01)      #join with sample name
colnames(ASV_by_sample)[1] <- "sample"               #rename column

#make a list of dataframes, if you get "Error: `n()` must only be used inside dplyr verbs." restart R. 
# There is a conflict with one of the packages from OccupancyModel.R

ASVs <- colnames(ASV_by_sample)
ASVs <- ASVs[-1]
ASVlist <- list()

for(i in ASVs){
  t1 <- dplyr::select(ASV_by_sample, sample, i)
  ASV <- t1 %>% 
    group_by(sample) %>%
    mutate(id = paste0("X", 1:n())) %>%
    spread(id, i)
  ASV$sample <- NULL
  ASVlist[[length(ASVlist)+1]] = ASV
}

ASVlist[1] #check to see that it is formatted properly

save(ASVlist, file="./Scripts/occupancy_modelling/royle_link/scratch/ASVlist_e.RData")
save(ASVs, file="./Scripts/occupancy_modelling/royle_link/scratch/ASVs_e.RData")


