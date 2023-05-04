#Explore Gamma (N/S) Comparisons

#Set-Up ####
#download packages 
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(dplyr)

#ALL SETS ####

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

#merge df together 
data2 <- merge(data, meta, by=c('set_number'))

#rename categories 
data2 <- data.frame(lapply(data2, function(x) { #both to both eDNA/trawl across whole dataset
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #trawl to only trawl across whole dataset
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change only eDNA to eDNA across whole dataset
  gsub("only eDNA", "eDNA", x)
  
}))

#format data to plot  (make categories T/F instead of characters)
data_long <- select(data2, c('LCT','set_number', 'beta_detection_method','leg'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, beta_detection_method, var)
data_wide[is.na(data_wide)] <- FALSE #replace NA with FALSE


#need to change TRUE in 'both' to TRUE in eDNA and trawl only 
#we combine both and not_both later
#subset only both 
data_wide_both <- subset(data_wide, both == TRUE)
data_wide_both$eDNA <- TRUE
data_wide_both$trawl <- TRUE

#subset NOT btoh 
data_wide_not_both <- subset(data_wide, both == FALSE)

#bind together
data_new <- rbind(data_wide_both, data_wide_not_both)
data_new <- select(data_new, c('leg', 'eDNA', 'trawl'))

#plot Euler diagram for N and S together 

plot <- plot(euler(data_new, by = list(leg)), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"), quantities=TRUE)
plot #this shows how many observations per category per region

plot <- plot(euler(data_new, by = list(leg)), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"))
plot

ggsave("./Outputs/diversity/south_north_euler_allsets.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

#Euler plots for S and N sites (species NOT observation comparisons) ####
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

#merge df together 
data2 <- merge(data, meta, by=c('set_number'))
data2 <- select(data2, c('beta_detection_method', 'LCT','leg'))
data2<- distinct(data2)

#rename categories 
data2 <- data.frame(lapply(data2, function(x) { #both to both eDNA/trawl across whole dataset
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #trawl to only trawl across whole dataset
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change only eDNA to eDNA across whole dataset
  gsub("only eDNA", "eDNA", x)
  
}))

#format data to plot  (make categories T/F instead of characters)
data_long <- select(data2, c('LCT', 'beta_detection_method','leg'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, beta_detection_method, var)
data_wide[is.na(data_wide)] <- FALSE #replace NA with FALSE


#need to change TRUE in 'both' to TRUE in eDNA and trawl only 
#we combine both and not_both later
#subset only both 
data_wide_both <- subset(data_wide, both == TRUE)
data_wide_both$eDNA <- TRUE
data_wide_both$trawl <- TRUE

#subset NOT btoh 
data_wide_not_both <- subset(data_wide, both == FALSE)

#bind together
data_new <- rbind(data_wide_both, data_wide_not_both)
data_new <- select(data_new, c('leg', 'eDNA', 'trawl'))

#plot Euler diagram for N and S together 

plot <- plot(euler(data_new, by = list(leg)), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"), quantities=TRUE)
plot #this shows how many observations per category per region

plot <- plot(euler(data_new, by = list(leg)), legend = TRUE, fills = c("#FCC442", "#5491cf", "#00AFBB"))
plot

ggsave("./Outputs/diversity/SN_euler_spp.png", 
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

#South region 
Strawl <- subset(trawl, trawl >= 7 ) #subset for only trawls in the southern sites 

SeDNA <- subset(eDNA, set_number >= 7) #subset for only eDNA samples in the southern sites 

df <- list(`eDNA` = c(SeDNA$LCT),
           `Trawl` = c(Strawl$LCT))

plot <- ggvenn(df,c("eDNA", "Trawl"), show_elements = T, label_sep = "\n", fill_color = c("#FCC442", "#5491cf","#00AFBB"), text_size = 3,
       fill_alpha=1.0) 
plot 

ggsave("./Outputs/diversity/south_venn.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


#North region 
Ntrawl <- subset(trawl, trawl <= 7 ) #subset for only trawls in the southern sites 

NeDNA <- subset(eDNA, set_number <= 7) #subset for only eDNA samples in the southern sites 

df <- list(`eDNA` = c(NeDNA$LCT),
           `Trawl` = c(Ntrawl$LCT))

plot <- ggvenn(df,c("eDNA", "Trawl"), show_elements = T, label_sep = "\n", fill_color = c("#FCC442", "#5491cf","#00AFBB"), text_size = 3,
       fill_alpha=1.0) 
plot 

ggsave("./Outputs/diversity/north_venn.png", 
       plot = plot,
       width = 6, height = 6, units = "in")



