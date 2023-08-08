#02.PCRdissimilarity95.R

#goal: remove samples where one or more PCR replicates has a distance to the sample centroid outside a 95% confidence interval

#install.packages("usedist")
#packages
library(here)
library(tidyverse)
library(vegan)
library(usedist)


#for SE
dat_se <- read.csv("./Processed_data/eDNA/12s/12s_e/asv/matrix/clean_data/data12Se_asvmatrix_metadata_nc.csv")

#extract usefull bits: t1 <- sample names; t2 <- ASV x site
t1 <- dat_se[c("sample_name")]
t2 <- dat_se[c(22:784)]
t3 <- cbind(t1, t2)

#calculate bray curtis dissimilarity
t4 <- vegdist(t3[2:764], method = "bray", binary = T)

# calculate distance from group centroid using Multivariate homogeneity of groups dispersions (variances)
t5 <- betadisper(t4, group = dat_se$sample_name, type = "centroid")

#plot with 95% CI elipses
 plot(t5, hull = F, ellipse = T, conf = 0.95, label = F, main = "95% CI elipse around sample replicates")

#extract distances
t6 <- cbind(dat_se$sample_name, as.data.frame(t5$distances))
colnames(t6) <- c("sample_name", "distances")

#calculate 95% CIs
t7 <- t6 %>%
  group_by(sample_name) %>%
  mutate(meandis=mean(distances), # mutate when grouped applies the summary to each row
         SDdis=sd(distances),
         CIdis = 1.96*SDdis/sqrt(3),            #use z = 2.05 for 96% CI
         upperCIdis=meandis+CIdis) #calculate upper end of 95% CI

#check that PCR reps are within CIs
t7$diff <- ifelse(t7$upperCIdis<t7$distances, "problem", "ok")
t8 <- filter(t7, diff == "problem")
t9 <- t8 %>% mutate(percent_off = ((distances - upperCIdis) / distances) * 100)

#for SU
dat_su <- read.csv("./Processed_data/eDNA/12s/12s_u/asv/matrix/clean_data/data12Su_asvmatrix_metadata_nc.csv")

#extract usefull bits: t1 <- sample names; t2 <- ASV x site
u1 <- dat_su[c("sample_name")]
u2 <- dat_su[c(22:667)]
u3 <- cbind(u1, u2)

#calculate bray curtis dissimilarity
u4 <- vegdist(u3[2:647], method = "bray", binary = T)

# calculate distance from group centroid using Multivariate homogeneity of groups dispersions (variances)
u5 <- betadisper(u4, group = dat_su $sample_name, type = "centroid")

#plot with 95% CI elipses
plot(u5, hull = F, ellipse = T, conf = 0.95, label = F, main = "95% CI elipse around sample replicates")

#extract distances
u6 <- cbind(dat_su $sample_name, as.data.frame(u5$distances))
colnames(u6) <- c("sample_name", "distances")

#calculate 95% CIs
u7 <- u6 %>%
  group_by(sample_name) %>%
  mutate(meandis=mean(distances), # mutate when grouped applies the summary to each row
         SDdis=sd(distances),
         CIdis = 1.96*SDdis/sqrt(3), 
         upperCIdis=meandis+CIdis) #calculate upper end of 95% CI

#check that PCR reps are within CIs
u7$diff <- ifelse(u7$upperCIdis<u7$distances, "problem", "ok")
u8 <- filter(u7, diff == "problem")
u9 <- u8 %>% mutate(percent_off = ((distances - upperCIdis) / distances) * 100)
