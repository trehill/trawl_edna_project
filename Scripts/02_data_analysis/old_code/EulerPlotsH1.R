#Hypothesis 1: 
#Exploring Gamma Diversity 
#Author: Tessa Rehill

#need to reassess 'both' datasets...
#Set-Up ####
library(dplyr)
library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)

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

#Creation of 3 datasets (trawl only, eDNA only, both) ####

#Find overlapping species found using both methods 
trawl_species <- trawl_data$species
eDNA_species <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_species, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$species %in% eDNA_species, ] 

#check that they are the same species
unique(botheDNA$LCT) 
unique(bothtrawl$species) #yay they are the same! 

#we need to find species only found in trawl 
bothtrawl2 <- bothtrawl$species

onlytrawl <- trawl_data[  !(trawl_data$species %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find species only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_data[  !(eDNA_data$LCT %in% botheDNA2), ]  #should have less than original eDNA df 

#Okay, now we have three datasets
#1. both eDNA and trawl (with respective datasets) - might have to merge this? 
#2. only eDNA 
#3. only trawl 

#Whole Dataset Exploration ####

##First, we'll explore whole datasets (irrelevant of N/S) 

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 
#eulerr
#euler only works on object class euler which has this format: 
#fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))

#select species only from datasets (since we are looking at the whole) 
a_value <- onlytrawl %>% 
            select(c("species")) %>%
            distinct() %>%
            count(species)

a <- sum(a_value$n)

b_value <- onlyeDNA %>% 
  select(c("LCT")) %>%
  distinct() %>%
  count(LCT)

b <- sum(b_value$n)

ab_value <- botheDNA %>% 
  select(c("LCT")) %>%
  distinct() %>%
  count(LCT)

ab <- sum(ab_value$n)

ab_value <- bothtrawl %>% 
  select(c("species")) %>%
  distinct() %>%
  count(species)

ab <- sum(ab_value$n) #same as above 

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

#Using ggVenn
df <- list(`Trawl` = c(trawl_data$species),
             +            `eDNA` = c(eDNA_data$LCT))
ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)

#North Gamma Comparison ####
#Okay, now lets distinguish between N + S regions 
#now we need to subset data by N and S 
#Let's start with the NORTH 

NbotheDNA <- botheDNA[botheDNA$north_south == 'N',]
Nbothtrawl <- bothtrawl[bothtrawl$region == 'northern',]
NonlyeDNA <- onlyeDNA[onlyeDNA$north_south == 'N',]
Nonlytrawl <- onlytrawl[onlytrawl$region == 'northern',]

#let's try eulerr 
a_value <- Nonlytrawl %>% 
  select(c("species")) %>%
  distinct() %>%
  count(species)

a <- sum(a_value$n)

b_value <- NonlyeDNA %>% 
  select(c("LCT")) %>%
  distinct() %>%
  count(LCT)

b <- sum(b_value$n)

ab_value <- NbotheDNA %>% 
  select(c("LCT")) %>%
  distinct() %>%
  count(LCT)

ab <- sum(ab_value$n) #4

#ab_value <- Nbothtrawl %>% 
  select(c("species")) %>%
  distinct() %>%
  count(species)

#ab <- sum(ab_value$n) #5, different but why, that means that we want to use the smaller number 

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

#Let's use Venn diagram to mark species names 
Ntrawl_data <- trawl_data[trawl_data$region == 'northern',]
NeDNA_data <- eDNA_data[eDNA_data$north_south == 'N',]

df <- list(`Trawl` = c(Ntrawl_data$species),
           `eDNA` = c(NeDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#South Gamma Comparison  ####

SbotheDNA <- botheDNA[botheDNA$north_south == 'S',]
Sbothtrawl <- bothtrawl[bothtrawl$region == 'southern',]
SonlyeDNA <- onlyeDNA[onlyeDNA$north_south == 'S',]
Sonlytrawl <- onlytrawl[onlytrawl$region == 'southern',]

#let's try eulerr 
a_value <- Sonlytrawl %>% 
  select(c("species")) %>%
  distinct() %>%
  count(species)

a <- sum(a_value$n)

b_value <- SonlyeDNA %>% 
  select(c("LCT")) %>%
  distinct() %>%
  count(LCT)

b <- sum(b_value$n)

#ab_value <- SbotheDNA %>% 
  select(c("LCT")) %>%
  distinct() %>%
  count(LCT)

#ab <- sum(ab_value$n) 

#ab #7 #bigger number than in trawl

ab_value <- Sbothtrawl %>% 
select(c("species")) %>%
  distinct() %>%
  count(species)

ab <- sum(ab_value$n) 

ab #6, this kind of makes no sense 

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE, legend = list(labels = c("trawl", "eDNA")) )

#Let's use Venn diagram to mark species names 
Strawl_data <- trawl_data[trawl_data$region == 'southern',]
SeDNA_data <- eDNA_data[eDNA_data$north_south == 'S',]

df <- list(`Trawl` = c(Strawl_data$species),
           `eDNA` = c(SeDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#Exploring beta diversity 
#I want to do something like this: 

plot(euler(fruits[, 1:4], by = list(sex)), legend = TRUE) #how to do this but with set?

#banana = trawl
#apple = eDNA
#sex = set 
#1,2,3 = species 

typeof(fruits)


matrix <- read.csv(here::here("Scripts", #should be ASV by sample
                                 "data_analysis",
                                 "eulerfake.csv"),
                      head=TRUE)

typeof(matrix)
plot(euler(matrix[, 1:4], by = list(set_number)), legend = TRUE) 

plot(euler(matrix[, 1:4], by = list(set_number)), legend = TRUE) #how to do this but with set?
