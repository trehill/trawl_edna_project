#Explore Gamma (N/S) Diversity 
#Set-Up ####

library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(dplyr)

#NORTHERN DIVERSITY ####

trawl_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  "fulldatasettrawlmin.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "eDNAfulldatasetmin.csv"),
                      head=TRUE)

#Creation of 3 datasets (trawl only, eDNA only, both) ####

#first we subset by north 

trawl_data <- trawl_data[trawl_data$region == 'northern',]
eDNA_data <- eDNA_data[eDNA_data$north_south == 'N',]

#Find overlapping species found using both methods 
trawl_species <- trawl_data$species
eDNA_species <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_species, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$species %in% eDNA_species, ] 

#check that they are the same species
unique(botheDNA$LCT) 
unique(bothtrawl$species) #yay they are the same! 

both = bothtrawl
#we need to find species only found in trawl 
bothtrawl2 <- bothtrawl$species

onlytrawl <- trawl_data[  !(trawl_data$species %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find species only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_data[  !(eDNA_data$LCT %in% botheDNA2), ]  #should have less than original eDNA df 

#Add a new column called method: 
#What I'm going to do is take each individual dataset and add a method that is either trawl,
#or eDNA and then I will merge them together

onlytrawl = onlytrawl$species
onlyeDNA = onlyeDNA$LCT
both = both$species

#let's do this with eDNA data 
eDNA_data$method = ifelse(eDNA_data$LCT %in% both , eDNA_data$method <- c("both"), eDNA_data$method <- c("eDNA"))

#let's do this with trawl data 
trawl_data$method = ifelse(trawl_data$species %in% both , trawl_data$method <- c("both"), trawl_data$method <- c("trawl"))

#merge datasets 
trawl_data <- select(trawl_data, c("species", "method"))
eDNA_data <- select(eDNA_data, c("LCT","method"))
colnames(eDNA_data) <- c('species','method')

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$species),
           `eDNA` = c(eDNA_data$species))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SOUTHERN DIVERSITY ####

trawl_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  "catch_data",
                                  "clean_data",
                                  "fulldatasettrawlmin.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "eDNAfulldatasetmin.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$region == 'southern',]
eDNA_data <- eDNA_data[eDNA_data$north_south == 'S',]

#Find overlapping species found using both methods 
trawl_species <- trawl_data$species
eDNA_species <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_species, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$species %in% eDNA_species, ] 

#check that they are the same species
unique(botheDNA$LCT) 
unique(bothtrawl$species) #yay they are the same! 

both = bothtrawl
#we need to find species only found in trawl 
bothtrawl2 <- bothtrawl$species

onlytrawl <- trawl_data[  !(trawl_data$species %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find species only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_data[  !(eDNA_data$LCT %in% botheDNA2), ]  #should have less than original eDNA df 

#Add a new column called method: 
#What I'm going to do is take each individual dataset and add a method that is either trawl,
#or eDNA and then I will merge them together

onlytrawl = onlytrawl$species
onlyeDNA = onlyeDNA$LCT
both = both$species

#let's do this with eDNA data 
eDNA_data$method = ifelse(eDNA_data$LCT %in% both , eDNA_data$method <- c("both"), eDNA_data$method <- c("eDNA"))

#let's do this with trawl data 
trawl_data$method = ifelse(trawl_data$species %in% both , trawl_data$method <- c("both"), trawl_data$method <- c("trawl"))

#merge datasets 
trawl_data <- select(trawl_data, c("species", "method"))
eDNA_data <- select(eDNA_data, c("LCT","method"))
colnames(eDNA_data) <- c('species','method')

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$species),
           `eDNA` = c(eDNA_data$species))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)
