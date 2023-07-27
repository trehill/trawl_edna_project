#Species count
#Author: Tessa Rehill 
#goal: #create incidences of each LCT in GAMMA diversity (across all) and BETA (across 
#north and south sites), create incidence boxplot at the gamma level 


#SET UP ####
#load libraries
library(tidyr)
library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)

#read in data 
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


#Gamma Level ####
#count
x <- data

count_spp <- x %>% 
  count(LCT) #count how many times a species is seen across the dataset 

#merge w/ original detection data 
data_count <- merge(x, count_spp, by=c('LCT'))

#extract only information we want 
df <- select(data_count, c('LCT','n','gamma_detection_method', 'beta_detection_method', 'alpha_detection_method')) #select columns 
df <- distinct(df) #make sure no duplicates

#write file to see! 
write_csv(df,
          here("Processed_data",
               "datasets",
               "diversity",
               "gamma_spp_count.csv")) 

#these numbers will be used to make the proportions for the spp. images in Fig. 1
#n= incidence 

#make table that includes the LCT, detection method (at gamma level), incidence, and mean biomass 

#Plot 

#at gamma level 

plot <- ggplot(df, aes(x=as.factor(gamma_detection_method), y=n)) + 
  geom_boxplot(fill= c("#00AFBB", "#FCC442","#5491cf"), alpha=1) +
  xlab("") + ylab("incidence") +
  theme_classic()  

 plot 
 
 
 ggsave("./Outputs/diversity/incidence_box.png", 
        plot = plot,
        width = 6, height = 6, units = "in")
 
 #at beta level 
 
 plot <- ggplot(df, aes(x=as.factor(beta_detection_method), y=n)) + 
   geom_boxplot(fill= c("#00AFBB","#FCC442", "#5491cf"), alpha=1) +
   xlab("") + ylab("incidence") +
   theme_classic()  
 
 plot 
 
 #at alpha level 
 
 plot <- ggplot(df, aes(x=as.factor(alpha_detection_method), y=n)) + 
   geom_boxplot(fill= c("#00AFBB", "#FCC442", "#5491cf"), alpha=1) +
   xlab("") + ylab("incidence") +
   theme_classic()  
 
 plot 
 
 #Beta Level  ####
 data2 <- merge(data, meta, by=c('set_number')) 
 
 #subset data by region 
 northern <-  data2[data2$leg == 'northern',]
 southern <-  data2[data2$leg == 'southern',]
 
 #what species were detected in each region? 
 unique(northern$LCT) #19 species total 
 
 unique(southern$LCT) #36 species total 
 
 #how many species in both at each region? 
 n_beta <-  northern[northern$beta_detection_method == 'both eDNA/trawl',]
 s_beta <-  southern[southern$beta_detection_method  == 'both eDNA/trawl',]
 
 unique(n_beta$LCT) #8 species total 
 unique(s_beta$LCT) #12 species total 
 
 #how many species only in eDNA in each region? 
 
 n_beta <-  northern[northern$beta_detection_method == 'only eDNA',]
 s_beta <-  southern[southern$beta_detection_method  == 'only eDNA',]
 
 unique(n_beta$LCT) #5 species total 
 
 unique(s_beta$LCT) #17 species total 
 
 #how many species only in trawl at each region?
 n_beta <-  northern[northern$beta_detection_method == 'only trawl',]
 s_beta <-  southern[southern$beta_detection_method  == 'only trawl',]
 
 unique(n_beta$LCT) #6 species total 
 
 unique(s_beta$LCT) #7 species total 
 
 #Find mean number of species with overlap between methods at different scales 
 
 #gamma level 
 
 gamma <- data[data$gamma_detection_method == 'both eDNA/trawl',]
 
 count_spp <- gamma %>% 
   count(LCT) 
 
 #site level 
 #i want to take the number of species detected by both methods and divide it by the number of species overall
 #found at this site, then take the mean of all those numbers 
 
spp_count <- data %>% 
  group_by(set_number) %>%
  count(set_number) %>%#n here is the number of sp/set 
  rename(set_count = n)

both <- data[data$alpha_detection_method == 'both eDNA/trawl',] #select for only sp found in both methods 

both_count <- both  %>%
  group_by(set_number) %>%
  count(set_number) %>% #n here is the number of sp/set IN BOTH 
  rename(spp_count = n)

overlap <- merge(spp_count, both_count, all.x=TRUE) #merge

overlap <- overlap %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) #replace all NA with 0 


#change NA to 0 

overlap$overlap_perc <- ((overlap$spp_count / overlap$set_count)*100)

mean(overlap$overlap_perc) #26.4% #percentage overlap by both methods

#find percentage overlap per set for eDNA metabarcoding 

eDNA <- subset(data, alpha_detection_method != c('only trawl')) #select for only sp found in eDNA

eDNA_count <- eDNA  %>%
  group_by(set_number) %>%
  count(set_number) %>% #n here is the number of sp/set IN BOTH 
  rename(spp_count = n)

overlap <- merge(spp_count, eDNA_count, all.x=TRUE) #merge

overlap <- overlap %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) #replace all NA with 0 

overlap$overlap_perc <- ((overlap$spp_count / overlap$set_count)*100)

mean(overlap$overlap_perc) #63% #percentage overlap by eDNA



