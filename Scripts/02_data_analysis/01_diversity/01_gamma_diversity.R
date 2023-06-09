#Exploring All Diversity 
#Author: Tessa Rehill
#goal: create Euler and Venn diagrams of species detections at the gamma level  
#additionally, check that all species detected uniquely in trawl appear in NCBI (manually)

#Set-Up ####
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(here)
library(dplyr)

#read data 
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

#Read in RAW data (this data separates eDNA species from trawl species - no 'both' category)
trawl<- read.csv(here::here("Processed_data", 
                            "trawl",
                            "catch_data", 
                            "clean_data",
                            "trawl_catch_clean.csv"),
                 head=TRUE)

eDNA <- read.csv(here::here("Processed_data", 
                            "eDNA",
                            "datasets", 
                            "eDNA_allsets.csv"),
                 head=TRUE)

#Quantitative EULER plots per observation ####

#format data to be able to plot 
#rename categories (just mae)
data2 <- data.frame(lapply(data, function(x) { #both to both eDNA/trawl across whole dataset
  gsub("both eDNA/trawl", "both", x)}))

data2 <- data.frame(lapply(data2, function(x) { #trawl to only trawl across whole dataset
  gsub("only trawl", "trawl", x)}))

data2 <- data.frame(lapply(data2, function(x) { #change only eDNA to eDNA across whole dataset
  gsub("only eDNA", "eDNA", x)}))

#make true/false for detections instead of character categories (necessary for Euler function)
#we do this in gamma_diversity, and in beta/alpha diversity scripts (method is consistent and the same!)
#in script annotations: Euler detection formatting method

data <- data2 #copy data 

data_long <- select(data, c('LCT','set_number', 'gamma_detection_method')) #select columns of interest 
data_long$var <- TRUE #add 'true' column, TRUE = present for this method

data_wide <- spread(data_long, gamma_detection_method, var) #make wide data
data_wide[is.na(data_wide)] <- FALSE #replace NA with FALSE, since it is not TRUE for this method 


#need to change TRUE in 'both' to TRUE in eDNA and trawl methods (detected in ALL)
#we subset and then bind together both + not both 
#subset only both 
data_wide_both <- subset(data_wide, both == TRUE)
data_wide_both$eDNA <- TRUE
data_wide_both$trawl <- TRUE

#subset NOT both
data_wide_not_both <- subset(data_wide, both == FALSE)

#bind together
data_new <- rbind(data_wide_both, data_wide_not_both) #has original number of rows as data_wide 
data_new <- select(data_new, c('eDNA', 'trawl')) #select only columns of interest, euler only take 2 NOT 3 columns TRUE/FALSE table

#now let's plot

#Comparison per observation 
plot <- plot(euler(data_new), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"), quantities=TRUE)
plot #this plot shows every single observation w/ quantities of the observation 
#total of 170 observations

plot <- plot(euler(data_new), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"))
plot #plot without number of observations per categories 


#save plot
ggsave("./Outputs/diversity/gamma_diversity_allsets.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


#Quantitative EULER plots per species ####
#instead of every observation (170) this just shows comparison by species 
#plot that shows not every single observation 

#Euler detection formatting method (as before): 
data <- data2 

data_long <- select(data, c('LCT','set_number', 'gamma_detection_method')) #select columns of interest 
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, gamma_detection_method, var) #make wide data
data_wide[is.na(data_wide)] <- FALSE #replace NA with FALSE

#need to change TRUE in 'both' to TRUE in eDNA and trawl only 
#subset only both 
data_wide_both <- subset(data_wide, both == TRUE)
data_wide_both$eDNA <- TRUE
data_wide_both$trawl <- TRUE

#subset NOT btoh 
data_wide_not_both <- subset(data_wide, both == FALSE)

#bind together
data_new <- rbind(data_wide_both, data_wide_not_both) 
data_new <- select(data_new, c('eDNA', 'trawl', 'LCT')) #select only columns of interest 
data4 <- distinct(data_new)
data_new <- select(data4, c('eDNA', 'trawl')) #select only columns of interest 


plot <- plot(euler(data_new), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"), quantities=TRUE, fill_alpha = 1.0)
plot #this is gamma diversity per species, with quantities of species

plot <- plot(euler(data_new), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"),  fill_alpha = 1.0)
plot #this is gamma diversity per species 

ggsave("./Outputs/diversity/gamma_species.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

#Qualitative Venn diagram showing species detection per method ####
#Let's try to see this plot qualitatively using ggvenn
#It also acts as a 'double' check that our Euler plots are showing us accurate results 

#make two lists that will be compared 
df <- list(`eDNA` = c(eDNA$LCT),
           `Trawl` = c(trawl$LCT))

#plot
plot <- ggvenn(df,c("eDNA", "Trawl"), show_elements = T, label_sep = "\n", fill_color = c("#FCC442", "#5491cf","#00AFBB"), text_size = 3,
               fill_alpha=1.0) 
plot


ggsave("./Outputs/diversity/all_venn.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#What species are only in the trawl- are they in NCBI? ####
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

#subset for species (in gamma detection) that are only in trawl 
trawl_spp <- subset(data, gamma_detection_method == 'only trawl')

unique(trawl_spp$LCT)


#Let's look these species up in NCBI 

#These species have 12s data; 
#Allosmerus elongatus 
#Engraulis mordax
#Diaphus theta
#Entosphenus tridentatus 
#Mallotus villosus 
#Parophrys vetulus
#Spirinchus starksi
#Ammodytes hexapterus 

#These species do NOT have 12s data in NCBI 
#Sebastes flavidus - does have cytochrom b oxidase records, many COI records in BOLD 
#Zoarcidae sp - COI records 


