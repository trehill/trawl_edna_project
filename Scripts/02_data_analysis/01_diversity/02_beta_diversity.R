#Explore Gamma (N/S) Diversity 
#Set-Up ####

library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(dplyr)

#EXCLUDED SETS ####
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections.csv"),
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

ggsave("./Outputs/diversity/south_north_euler.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


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

plot

#OLD CODE ####

#NORTHERN DIVERSITY 

trawl_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  'datasets',
                                  "fulldatasettrawl.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 'datasets',
                                 "eDNAfulldataset.csv"),
                      head=TRUE)

#first we subset by north 

trawl_data <- trawl_data[trawl_data$north_south == 'northern',]
eDNA_data <- eDNA_data[eDNA_data$north_south == 'N',]

#Find overlapping species found using both methods 
trawl_species <- trawl_data$LCT
eDNA_species <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_species, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_species, ] 

#check that they are the same species
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find species only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find species only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_data[  !(eDNA_data$LCT %in% botheDNA2), ]  #should have less than original eDNA df 

#Add a new column called method: 
#What I'm going to do is take each individual dataset and add a method that is either trawl,
#or eDNA and then I will merge them together

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_data$method = ifelse(eDNA_data$LCT %in% both , eDNA_data$method <- c("both"), eDNA_data$method <- c("eDNA"))

#let's do this with trawl data 
trawl_data$method = ifelse(trawl_data$LCT %in% both , trawl_data$method <- c("both"), trawl_data$method <- c("trawl"))

#merge datasets 
trawl_data <- select(trawl_data, c("LCT", "method"))
eDNA_data <- select(eDNA_data, c("LCT","method"))
colnames(eDNA_data) <- c('LCT','method')

data <- rbind(trawl_data, eDNA_data)
data <- distinct(data)

#remove na. 


#Okay, now we have our data! 

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(data$method==c("trawl")))
b <- length(which(data$method==c("eDNA")))
ab <- length(which(data$method==c("both")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))


plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot

ggsave("./Outputs/diversity/north_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot
ggvenn(df)

ggsave("./Outputs/diversity/north_venn.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

#SOUTHERN DIVERSITY 
trawl_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  'datasets',
                                  "fulldatasettrawl.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 'datasets',
                                 "eDNAfulldataset.csv"),
                      head=TRUE)

#first we subset by south

trawl_data <- trawl_data[trawl_data$north_south == 'southern',]
eDNA_data <- eDNA_data[eDNA_data$north_south == 'S',]

#Find overlapping species found using both methods 
trawl_species <- trawl_data$LCT
eDNA_species <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_species, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_species, ] 

#check that they are the same species
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find species only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find species only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_data[  !(eDNA_data$LCT %in% botheDNA2), ]  #should have less than original eDNA df 

#Add a new column called method: 
#What I'm going to do is take each individual dataset and add a method that is either trawl,
#or eDNA and then I will merge them together

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_data$method = ifelse(eDNA_data$LCT %in% both , eDNA_data$method <- c("both"), eDNA_data$method <- c("eDNA"))

#let's do this with trawl data 
trawl_data$method = ifelse(trawl_data$LCT %in% both , trawl_data$method <- c("both"), trawl_data$method <- c("trawl"))

#merge datasets 
trawl_data <- select(trawl_data, c("LCT", "method"))
eDNA_data <- select(eDNA_data, c("LCT","method"))
colnames(eDNA_data) <- c('LCT','method')

data <- rbind(trawl_data, eDNA_data)
data <- distinct(data)

#remove na. 


#Okay, now we have our data! 

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(data$method==c("trawl")))
b <- length(which(data$method==c("eDNA")))
ab <- length(which(data$method==c("both")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))


plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot

ggsave("./Outputs/diversity/south_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot
ggvenn(df)

ggsave("./Outputs/diversity/south_venn.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


