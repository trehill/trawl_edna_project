#Exploring All Diversity 
#Author: Tessa Rehill

#need to reassess 'both' datasets...
#Set-Up ####
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(here)
library(dplyr)


#ALLSETS####

data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

data2 <- data.frame(lapply(data, function(x) { #change southern to S across all df
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only eDNA", "eDNA", x)
  
}))

#plot 
data_long <- select(data2, c('LCT','set_number', 'gamma_detection_method'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, gamma_detection_method, var)
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
data_new <- select(data_new, c('eDNA', 'trawl'))

#switch TRUE + FALSE 

plot <- plot(euler(data_new), legend = TRUE, fills = c("#fef3da", "#ebf9fa","#dbe8f5"))

plot

#save plot
ggsave("./Outputs/diversity/gamma_diversity_allsets.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

