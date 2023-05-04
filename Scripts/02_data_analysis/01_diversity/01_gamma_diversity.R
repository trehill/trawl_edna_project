#Exploring All Diversity 
#Author: Tessa Rehill

#Set-Up ####
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(here)
library(dplyr)

#read data 
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

#ALLSETS####

#format data to be able to plot 
#rename categories 
data2 <- data.frame(lapply(data, function(x) { #both to both eDNA/trawl across whole dataset
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #trawl to only trawl across whole dataset
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change only eDNA to eDNA across whole dataset
  gsub("only eDNA", "eDNA", x)
  
}))

#make true/false for detections instead of character categories 
data_long <- select(data2, c('LCT','set_number', 'gamma_detection_method')) #select columns of interest 
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, gamma_detection_method, var) #make wide data
data_wide[is.na(data_wide)] <- FALSE #replace NA with FALSE


#need to change TRUE in 'both' to TRUE in eDNA and trawl only 
#we will then bind the two sets together (both + not both)
#subset only both 
data_wide_both <- subset(data_wide, both == TRUE)
data_wide_both$eDNA <- TRUE
data_wide_both$trawl <- TRUE

#subset NOT both
data_wide_not_both <- subset(data_wide, both == FALSE)

#bind together
data_new <- rbind(data_wide_both, data_wide_not_both) #has original number of rows as data_wide 
data_new <- select(data_new, c('eDNA', 'trawl')) #select only columns of interest 

#now let's plot

#Comparison per observation 
plot <- plot(euler(data_new), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"), quantities=TRUE)


plot #this plot shows every single observation w/ quantities of the observation 


plot <- plot(euler(data_new), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"))

plot #plot without number of observations per categories 


#save plot
ggsave("./Outputs/diversity/gamma_diversity_allsets.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


#Comparison per species 
#instead of every observation (169) this just shows comparison by species 
#plot that shows not every single observation 
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

data2 <- data.frame(lapply(data, function(x) { #both to both eDNA/trawl across whole dataset
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #trawl to only trawl across whole dataset
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change only eDNA to eDNA across whole dataset
  gsub("only eDNA", "eDNA", x)
  
}))

#plot 
data_long <- select(data2, c('LCT','set_number', 'gamma_detection_method')) #select columns of interest 
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
plot #this is gamma diversity per species 

plot <- plot(euler(data_new), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"),  fill_alpha = 1.0)
plot #this is gamma diversity per species 

ggsave("./Outputs/diversity/gamma_species.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

#Qualitative Venn diagram showing species detection per method 
#Let's try to see this plot qualitatively using ggvenn

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

