#Mean Biomass of Species 
#goal: calculates the mean biomass per and creates euler plot for data visualization

#SET UP 
library(eulerr)
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)

#read data
data <- read.csv(here::here("Processed_data", 
                            "biomass",
                            "biomass_all.csv"),
                 head=TRUE)

#MEAN BIOMASS PER SPECIES ####

x <- data

new <- x %>%
  group_by(LCT) %>%
  dplyr::summarise(mean_biomass = mean(biomass_index))

#merge w/ big data 
data_count <- merge(x, new, by=c('LCT'))

#extract only information we want 

df <- select(data_count, c('LCT','mean_biomass','gamma_detection_method')) #select columns
df <- distinct(df) #ensure no replicates 

write_csv(df,
          here("Processed_data",
               "biomass",
               "species_biomass_all.csv")) 

#SUM BIOMASS PER SPECIES ####
x <- data

new <- x %>%
  group_by(LCT) %>%
  dplyr::summarise(sum_biomass = sum(biomass_index))

data_count <- merge(x, new, by=c('LCT'))

df <- select(data_count, c('LCT','sum_biomass','gamma_detection_method')) #select columns
df <- distinct(df) #ensure no replicates

write_csv(df,
          here("Processed_data",
               "biomass",
               "species_biomass_sum_all.csv")) 

#EULER PLOT ####
#make euler plot with only both + trawl categories 

data <- read.csv(here::here("Processed_data", 
                            "biomass",
                            "biomass_all.csv"),
                 head=TRUE)

#rename categorical variables to binary TRUE/FALSE 
data2 <- data.frame(lapply(data, function(x) { #change southern to S across all df
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only eDNA", "eDNA", x)
  
}))

data2 <- subset(data2, gamma_detection_method == c('trawl', 'both'))

#format
data_long <- select(data2, c('LCT','set_number', 'gamma_detection_method'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, gamma_detection_method, var)
data_wide[is.na(data_wide)] <- FALSE #replace NA with FALSE


#need to change TRUE in 'both' to TRUE in eDNA and trawl only 
#subset only both 
data_wide_both <- subset(data_wide, both == TRUE)
data_wide_both$trawl <- TRUE

#subset NOT both 
data_wide_not_both <- subset(data_wide, both == FALSE)

#bind together
data_new <- rbind(data_wide_both, data_wide_not_both)
data_new <- select(data_new, c('both', 'trawl'))

#plot
plot <- plot(euler(data_new), legend = TRUE, fills = c("#00AFBB", "#5491cf"))

plot

#save plot
ggsave("./Outputs/biomass/eulerbiomass_all.png", 
       plot = plot,
       width = 6, height = 6, units = "in")





