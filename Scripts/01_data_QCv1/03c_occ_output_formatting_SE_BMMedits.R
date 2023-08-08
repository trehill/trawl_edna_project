#03c_occ_output_formatting.R

library(here)
#### Summarise "asv_by_site" from wrangling.R; count# of detections per sample ####

out <- read.csv("./Scripts/occupancy_modelling/royle_link/scratch/occProb_royallink_e.csv")
rownames(out) <- out$X
out$X <- NULL
occprob <- out  

t1 <- ASV_by_sample %>%
  group_by(sample) 
t2 <- summarise_all(t1,list(sum))
t2 <- column_to_rownames(t2, "sample")
t3 <- t(t2)
t3 <- as.data.frame(t3)
sumreps <- t3

#### assign occupancy probability given # of detections ####
occprob_by_sample <- ifelse(sumreps == 3, occprob$PoO.3, ifelse(
  sumreps == 2, occprob$PoO.2, ifelse(
    sumreps == 1, occprob$PoO.1, occprob$PoO.0)))

write.csv(occprob_by_sample, "./Outputs/occupancy_modelling/royle_link/occprob_by_sample_e.csv")

# find all ASVs for which occprob is always less than 0.8 ... or 0.2
t9 <- ifelse(occprob_by_sample >= 0.8, 1, 0)
#write.csv(t9, "./Outputs/occupancy_modelling/royle_link/occ_by_sample.csv")
t10 <- as.data.frame(t(t9))
ASVcount_a <- as.data.frame(colSums(t10))           #find number of observations of each ASV
ASV0count_a <- filter(ASVcount_a, ASVcount_a[1] == 0)      #find ASVs with 0 observations
ASVswith0count_a <- rownames(ASV0count_a)
t10[,c(ASVswith0count_a)] <- NULL                   #remove ASVs with 0 "true positive" observations from ASV matrix
t15 <- as.matrix(t10)

#format ASV read abundance x site
t11 <- spec[c(colnames(t10))]                       #filter ASVs that have occupancy probability above threshold (defined above)
t12 <- cbind(dat$sample_name, t11)
colnames(t12)[1] <- "sample"                   #sum PCR read counts across samples
t13 <- t12 %>%
  group_by(sample) %>%
  summarise_all(list(sum)) %>%
  column_to_rownames("sample")
t16 <- as.matrix(t13)

ASV_LowOccRemoved <- as.data.frame(ifelse(t15 == 0, 0, t16)) # filter ASV read number matrix (t16) with occupancy thresolds (t15)

write.csv(ASV_LowOccRemoved, "./Scripts/occupancy_modelling/royle_link/scratch/data12se_asvmatrix_lor_12s_e.csv")

#another file path to export data to

write.csv(ASV_LowOccRemoved, "./Processed_data/eDNA/12s/12s_e/asv/matrix/clean_data/data12se_asvmatrix_nc_lor.csv")
