#Put species in order of increasing biomass

#SET UP 
library(eulerr)
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)


data <- read.csv(here::here("Processed_data", 
                            "traits",
                            "biomass.csv"),
                 head=TRUE)

#COUNT ####
#count how many times a species is seen across the dataset 

x <- data

new <- x %>%
  group_by(LCT) %>%
  dplyr::summarise(mean_biomass = mean(biomass_index))


#merge w/ big data 

data_count <- merge(x, new, by=c('LCT'))

#extract only information we want 

df <- select(data_count, c('LCT','mean_biomass','gamma_detection_method'))
df <- distinct(df)

write_csv(df,
          here("Processed_data",
               "traits",
               "species_biomass.csv")) 

#PLOT ####
#make euler plot with only both + trawl categories 

data <- read.csv(here::here("Processed_data", 
                            "traits",
                            "biomass.csv"),
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

#subset NOT btoh 
data_wide_not_both <- subset(data_wide, both == FALSE)

#bind together
data_new <- rbind(data_wide_both, data_wide_not_both)
data_new <- select(data_new, c('both', 'trawl'))


#switch TRUE + FALSE 

plot <- plot(euler(data_new), legend = TRUE)

plot

#save plot
ggsave("./Outputs/biomass/eulerbiomass.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

