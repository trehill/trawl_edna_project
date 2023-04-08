#Explore Gamma (N/S) Diversity 
#Set-Up ####

library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(dplyr)




#ALL SETS ####

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

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only eDNA", "eDNA", x)
  
}))


#plot 
data_long <- select(data2, c('LCT','set_number', 'beta_detection_method','leg'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, beta_detection_method, var)
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
data_new <- select(data_new, c('leg', 'eDNA', 'trawl'))

#switch TRUE + FALSE 

plot <- plot(euler(data_new, by = list(leg)), legend = TRUE, fills = c("#fef3da", "#ebf9fa","#dbe8f5"))

plot

ggsave("./Outputs/diversity/south_north_euler_allsets.png", 
       plot = plot,
       width = 6, height = 6, units = "in")



