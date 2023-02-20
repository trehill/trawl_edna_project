#Making a dataset for analysis on betadiversity by adding detection 
#method at gamma, beta and alpha levels 

#we want to extract; 
# LCT 
# abundance info (for both + trawl method)
# read index (for both + eDNA method)
# set
# method (at site + gamma level)

#SETUP ####
library(plyr)
library(readr)
library(here)
library(dplyr)

#read in files 

#make eDNA df 
eDNA_df <-  read.csv(here::here("Processed_data",
                                         "eDNA",
                                          "datasets",
                                         "eDNAfulldataset.csv"), #has eDNA index reads
                              head=TRUE)

eDNA_df <- eDNA_df %>% #rename depth column 
  rename(
    depth_eDNA = depth)

eDNA_df <- eDNA_df %>% #rename presence/absence column 
  rename(
    pabs_eDNA = species_pa)

#remove species_pa = NA and index = NA
eDNA_df <- eDNA_df[!is.na(eDNA_df$pabs_eDNA),]
eDNA_df <- eDNA_df[!is.na(eDNA_df$set_read_index),]

#make trawl_df 
trawl_df <-  read.csv(here::here("Processed_data",
                             "trawl",
                             "datasets",
                             "trawl_catch_weight.csv"), #has weight
                  head=TRUE)

trawl_df <- trawl_df %>% #rename depth column 
  rename(
    depth_trawl = depth_mean)

trawl_df <- cbind(trawl_df, pabs_trawl = 1) #add column with value 1 to indicate presence/absence in trawl df 

trawl_df <- data.frame(lapply(trawl_df, function(x) { #change southern to S across all df
  gsub("southern", "S", x)
  
}))

trawl_df <- data.frame(lapply(trawl_df, function(x) { #change northern to N across all df
  gsub("northern", "N", x)
  
}))


#Selecting relevant columns ####

eDNA <- select(eDNA_df, c('LCT', 'set_number', 'north_south' ))
eDNA <- distinct(eDNA)

trawl <- select(trawl_df, c('LCT', 'set_number', 'north_south'  ))
trawl <- distinct(trawl)

#Gamma Detection ####
#let's start with gamma detection)
#determining gamma error (across whole dataset
#Find overlapping LCT found using both methods 
trawl_LCT <- trawl$LCT
eDNA_LCT <- eDNA$LCT


botheDNA <- eDNA[  eDNA$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl[  trawl$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl[  !(trawl$LCT %in% bothtrawl2), ] #should be less than original trawl df 
onlytrawl <- na.omit(onlytrawl)
#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA[  !(eDNA$LCT %in% botheDNA2), ]  #should have less than original eDNA df 

#Add a new column called method: 
#What I'm going to do is take each individual dataset and add a method that is either trawl,
#or eDNA and then I will merge them together

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA$gamma_detection_method = ifelse(eDNA$LCT %in% both , eDNA$gamma_detection_method <- c("both eDNA/trawl"), eDNA$gamma_detection_method<- c("only eDNA"))

#let's do this with trawl data 
trawl$gamma_detection_method = ifelse(trawl$LCT %in% both , trawl$gamma_detection_method <- c("both eDNA/trawl"), trawl$gamma_detection_method <- c("only trawl"))

#Beta Detection ####

#determining beta error (across regions N/S) 
#northern region
trawl_N <- trawl[trawl$north_south == 'N',]
eDNA_N <- eDNA[eDNA$north_south == 'N',]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_N$LCT
eDNA_LCT <- eDNA_N$LCT

botheDNA <- eDNA_N[  eDNA_N$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_N[  trawl_N$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_N[  !(trawl_N$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_N[  !(eDNA_N$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_N$beta_detection_method = ifelse(eDNA_N$LCT %in% both , eDNA_N$beta_detection_method <- c("both eDNA/trawl"), eDNA_N$beta_detection_method <- c("only eDNA"))
eDNA_N <- merge(eDNA, eDNA_N, by= c('set_number', 'LCT'))
eDNA_N <- distinct(eDNA_N)

#let's do this with trawl data 
trawl_N$beta_detection_method = ifelse(trawl_N$LCT %in% both , trawl_N$beta_detection_method <- c("both eDNA/trawl"), trawl_N$beta_detection_method <- c("only trawl"))
trawl_N <- merge(trawl, trawl_N, by= c('set_number', 'LCT'))
trawl_N <- distinct(trawl_N)

#southern
trawl_S <- trawl[trawl$north_south == 'S',]
eDNA_S <- eDNA[eDNA$north_south == 'S',]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_S$LCT
eDNA_LCT <- eDNA_S$LCT

botheDNA <- eDNA_S[  eDNA_S$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_S[  trawl_S$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_S[  !(trawl_S$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_S[  !(eDNA_S$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_S$beta_detection_method = ifelse(eDNA_S$LCT %in% both , eDNA_S$beta_detection_method <- c("both eDNA/trawl"), eDNA_S$beta_detection_method <- c("only eDNA"))
eDNA_S <- merge(eDNA, eDNA_S, by= c('set_number', 'LCT'))
eDNA_S <- distinct(eDNA_S)

#let's do this with trawl data 
trawl_S$beta_detection_method = ifelse(trawl_S$LCT %in% both , trawl_S$beta_detection_method <- c("both eDNA/trawl"), trawl_S$beta_detection_method <- c("only trawl"))
trawl_S <- merge(trawl, trawl_S, by= c('set_number', 'LCT'))
trawl_S <- distinct(trawl_S)

#merge to eDNA + trawl 
trawl_beta <- rbind(trawl_S,trawl_N)
eDNA_beta <- rbind(eDNA_S, eDNA_N)

#clean up dataset 

eDNA <- select(eDNA_beta, c('LCT', 'set_number', 'north_south.x', 'gamma_detection_method.x', 'beta_detection_method') )
trawl <- select(trawl_beta, c('LCT', 'set_number', 'north_south.x', 'gamma_detection_method.x', 'beta_detection_method') )

#Alpha Detection ####
#the hardest part of this code is going to be adding an set error + gammma error column 
#we can find gamma error from 'method_key_all' 
#set level error can be determined from our beta diversity code (iterative)

#determine beta detection methods
#set 1
trawl_1 <- trawl[trawl$set_number == 1,]
eDNA_1 <- eDNA[eDNA$set_number == 1,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_1$LCT
eDNA_LCT <- eDNA_1$LCT

botheDNA <- eDNA_1[  eDNA_1$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_1[  trawl_1$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_1[  !(trawl_1$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_1[  !(eDNA_1$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_1$alpha_detection_method = ifelse(eDNA_1$LCT %in% both , eDNA_1$alpha_detection_method <- c("both eDNA/trawl"), eDNA_1$alpha_detection_method <- c("only eDNA"))
eDNA_1 <- merge(eDNA, eDNA_1, by= c('set_number', 'LCT'))
eDNA_1 <- distinct(eDNA_1)

#let's do this with trawl data 
trawl_1$alpha_detection_method = ifelse(trawl_1$LCT %in% both , trawl_1$alpha_detection_method <- c("both eDNA/trawl"), trawl_1$alpha_detection_method <- c("only trawl"))
trawl_1 <- merge(trawl, trawl_1, by= c('set_number', 'LCT'))
trawl_1 <- distinct(trawl_1)

#set 2 
trawl_2 <- trawl[trawl$set_number == 2,]
eDNA_2 <- eDNA[eDNA$set_number == 2,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_2$LCT
eDNA_LCT <- eDNA_2$LCT

botheDNA <- eDNA_2[  eDNA_2$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_2[  trawl_2$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_2[  !(trawl_2$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_2[  !(eDNA_2$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_2$alpha_detection_method = ifelse(eDNA_2$LCT %in% both , eDNA_2$alpha_detection_method <- c("both eDNA/trawl"), eDNA_2$alpha_detection_method <- c("only eDNA"))
eDNA_2 <- merge(eDNA, eDNA_2, by= c('set_number', 'LCT'))
eDNA_2 <- distinct(eDNA_2)

#let's do this with trawl data 
trawl_2$alpha_detection_method = ifelse(trawl_2$LCT %in% both , trawl_2$alpha_detection_method <- c("both eDNA/trawl"), trawl_2$alpha_detection_method <- c("only trawl"))
trawl_2 <- merge(trawl, trawl_2, by= c('set_number', 'LCT'))
trawl_2 <- distinct(trawl_2)

#set 3
trawl_3 <- trawl[trawl$set_number == 3,]
eDNA_3 <- eDNA[eDNA$set_number == 3,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_3$LCT
eDNA_LCT <- eDNA_3$LCT

botheDNA <- eDNA_3[  eDNA_3$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_3[  trawl_3$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_3[  !(trawl_3$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_3[  !(eDNA_3$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_3$alpha_detection_method = ifelse(eDNA_3$LCT %in% both , eDNA_3$alpha_detection_method <- c("both eDNA/trawl"), eDNA_3$alpha_detection_method <- c("only eDNA"))
eDNA_3 <- merge(eDNA, eDNA_3, by= c('set_number', 'LCT'))
eDNA_3 <- distinct(eDNA_3)

#let's do this with trawl data 
trawl_3$alpha_detection_method = ifelse(trawl_3$LCT %in% both , trawl_3$alpha_detection_method <- c("both eDNA/trawl"), trawl_3$alpha_detection_method <- c("only trawl"))
trawl_3 <- merge(trawl, trawl_3, by= c('set_number', 'LCT'))
trawl_3 <- distinct(trawl_3)

#set 4
trawl_4 <- trawl[trawl$set_number == 4,]
eDNA_4 <- eDNA[eDNA$set_number == 4,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_4$LCT
eDNA_LCT <- eDNA_4$LCT

botheDNA <- eDNA_4[  eDNA_4$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_4[  trawl_4$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_4[  !(trawl_4$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_4[  !(eDNA_4$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_4$alpha_detection_method = ifelse(eDNA_4$LCT %in% both , eDNA_4$alpha_detection_method <- c("both eDNA/trawl"), eDNA_4$alpha_detection_method <- c("only eDNA"))
eDNA_4 <- merge(eDNA, eDNA_4, by= c('set_number', 'LCT'))
eDNA_4 <- distinct(eDNA_4)

#let's do this with trawl data 
trawl_4$alpha_detection_method = ifelse(trawl_4$LCT %in% both , trawl_4$alpha_detection_method <- c("both eDNA/trawl"), trawl_4$alpha_detection_method <- c("only trawl"))
trawl_4 <- merge(trawl, trawl_4, by= c('set_number', 'LCT'))
trawl_4 <- distinct(trawl_4)


#set 5
#set 1
trawl_5 <- trawl[trawl$set_number == 5,]
eDNA_5 <- eDNA[eDNA$set_number == 5,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_5$LCT
eDNA_LCT <- eDNA_5$LCT

botheDNA <- eDNA_5[  eDNA_5$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_5[  trawl_5$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_5[  !(trawl_5$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_5[  !(eDNA_5$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_5$alpha_detection_method = ifelse(eDNA_5$LCT %in% both , eDNA_5$alpha_detection_method <- c("both eDNA/trawl"), eDNA_5$alpha_detection_method <- c("only eDNA"))
eDNA_5 <- merge(eDNA, eDNA_5, by= c('set_number', 'LCT'))
eDNA_5 <- distinct(eDNA_5)

#let's do this with trawl data 
trawl_5$alpha_detection_method = ifelse(trawl_5$LCT %in% both , trawl_5$alpha_detection_method <- c("both eDNA/trawl"), trawl_5$alpha_detection_method <- c("only trawl"))
trawl_5 <- merge(trawl, trawl_5, by= c('set_number', 'LCT'))
trawl_5 <- distinct(trawl_5)

#set 7 
trawl_7 <- trawl[trawl$set_number == 7,]
eDNA_7 <- eDNA[eDNA$set_number == 7,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_7$LCT
eDNA_LCT <- eDNA_7$LCT

botheDNA <- eDNA_7[  eDNA_7$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_7[  trawl_7$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_7[  !(trawl_7$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_7[  !(eDNA_7$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_7$alpha_detection_method = ifelse(eDNA_7$LCT %in% both , eDNA_7$alpha_detection_method <- c("both eDNA/trawl"), eDNA_7$alpha_detection_method <- c("only eDNA"))
eDNA_7 <- merge(eDNA, eDNA_7, by= c('set_number', 'LCT'))
eDNA_7 <- distinct(eDNA_7)

#let's do this with trawl data 
trawl_7$alpha_detection_method = ifelse(trawl_7$LCT %in% both , trawl_7$alpha_detection_method <- c("both eDNA/trawl"), trawl_7$alpha_detection_method <- c("only trawl"))
trawl_7 <- merge(trawl, trawl_7, by= c('set_number', 'LCT'))
trawl_7 <- distinct(trawl_7)

#set 8 
trawl_8 <- trawl[trawl$set_number == 8,]
eDNA_8 <- eDNA[eDNA$set_number == 8,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_8$LCT
eDNA_LCT <- eDNA_8$LCT

botheDNA <- eDNA_8[  eDNA_8$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_8[  trawl_8$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_8[  !(trawl_8$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_8[  !(eDNA_8$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_8$alpha_detection_method = ifelse(eDNA_8$LCT %in% both , eDNA_8$alpha_detection_method <- c("both eDNA/trawl"), eDNA_8$alpha_detection_method <- c("only eDNA"))
eDNA_8 <- merge(eDNA, eDNA_8, by= c('set_number', 'LCT'))
eDNA_8 <- distinct(eDNA_8)

#let's do this with trawl data 
trawl_8$alpha_detection_method = ifelse(trawl_8$LCT %in% both , trawl_8$alpha_detection_method <- c("both eDNA/trawl"), trawl_8$alpha_detection_method <- c("only trawl"))
trawl_8 <- merge(trawl, trawl_8, by= c('set_number', 'LCT'))
trawl_8 <- distinct(trawl_8)

#set 9 
trawl_9 <- trawl[trawl$set_number == 9,]
eDNA_9 <- eDNA[eDNA$set_number == 9,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_9$LCT
eDNA_LCT <- eDNA_9$LCT

botheDNA <- eDNA_9[  eDNA_9$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_9[  trawl_9$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_9[  !(trawl_9$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_9[  !(eDNA_9$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_9$alpha_detection_method = ifelse(eDNA_9$LCT %in% both , eDNA_9$alpha_detection_method <- c("both eDNA/trawl"), eDNA_9$alpha_detection_method <- c("only eDNA"))
eDNA_9 <- merge(eDNA, eDNA_9, by= c('set_number', 'LCT'))
eDNA_9 <- distinct(eDNA_9)

#let's do this with trawl data 
trawl_9$alpha_detection_method = ifelse(trawl_9$LCT %in% both , trawl_9$alpha_detection_method <- c("both eDNA/trawl"), trawl_9$alpha_detection_method <- c("only trawl"))
trawl_9 <- merge(trawl, trawl_9, by= c('set_number', 'LCT'))
trawl_9 <- distinct(trawl_9)

#set 10 
trawl_10 <- trawl[trawl$set_number == 10,]
eDNA_10 <- eDNA[eDNA$set_number == 10,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_10$LCT
eDNA_LCT <- eDNA_10$LCT

botheDNA <- eDNA_10[  eDNA_10$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_10[  trawl_10$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_10[  !(trawl_10$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_10[  !(eDNA_10$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_10$alpha_detection_method = ifelse(eDNA_10$LCT %in% both , eDNA_10$alpha_detection_method <- c("both eDNA/trawl"), eDNA_10$alpha_detection_method <- c("only eDNA"))
eDNA_10 <- merge(eDNA, eDNA_10, by= c('set_number', 'LCT'))
eDNA_10 <- distinct(eDNA_10)

#let's do this with trawl data 
trawl_10$alpha_detection_method = ifelse(trawl_10$LCT %in% both , trawl_10$alpha_detection_method <- c("both eDNA/trawl"), trawl_10$alpha_detection_method <- c("only trawl"))
trawl_10 <- merge(trawl, trawl_10, by= c('set_number', 'LCT'))
trawl_10 <- distinct(trawl_10)

#set 11
trawl_11 <- trawl[trawl$set_number == 11,]
eDNA_11 <- eDNA[eDNA$set_number == 11,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_11$LCT
eDNA_LCT <- eDNA_11$LCT

botheDNA <- eDNA_11[  eDNA_11$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_11[  trawl_11$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_11[  !(trawl_11$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_11[  !(eDNA_11$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_11$alpha_detection_method = ifelse(eDNA_11$LCT %in% both , eDNA_11$alpha_detection_method <- c("both eDNA/trawl"), eDNA_11$alpha_detection_method <- c("only eDNA"))
eDNA_11 <- merge(eDNA, eDNA_11, by= c('set_number', 'LCT'))
eDNA_11 <- distinct(eDNA_1)

#let's do this with trawl data 
trawl_11$alpha_detection_method = ifelse(trawl_11$LCT %in% both , trawl_11$alpha_detection_method <- c("both eDNA/trawl"), trawl_11$alpha_detection_method <- c("only trawl"))
trawl_11 <- merge(trawl, trawl_11, by= c('set_number', 'LCT'))
trawl_11 <- distinct(trawl_11)

#set 12
trawl_12 <- trawl[trawl$set_number == 12,]
eDNA_12 <- eDNA[eDNA$set_number == 12,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_12$LCT
eDNA_LCT <- eDNA_12$LCT

botheDNA <- eDNA_12[  eDNA_12$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_12[  trawl_12$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_12[  !(trawl_12$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_12[  !(eDNA_12$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_12$alpha_detection_method = ifelse(eDNA_12$LCT %in% both , eDNA_12$alpha_detection_method <- c("both eDNA/trawl"), eDNA_12$alpha_detection_method <- c("only eDNA"))
eDNA_12 <- merge(eDNA, eDNA_12, by= c('set_number', 'LCT'))
eDNA_12 <- distinct(eDNA_12)

#let's do this with trawl data 
trawl_12$alpha_detection_method = ifelse(trawl_12$LCT %in% both , trawl_12$alpha_detection_method <- c("both eDNA/trawl"), trawl_12$alpha_detection_method <- c("only trawl"))
trawl_12 <- merge(trawl, trawl_12, by= c('set_number', 'LCT'))
trawl_12 <- distinct(trawl_12)

#set 13
trawl_13 <- trawl[trawl$set_number == 13,]
eDNA_13 <- eDNA[eDNA$set_number == 13,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_13$LCT
eDNA_LCT <- eDNA_13$LCT

botheDNA <- eDNA_13[  eDNA_13$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_13[  trawl_13$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_13[  !(trawl_13$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_13[  !(eDNA_13$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_13$alpha_detection_method = ifelse(eDNA_13$LCT %in% both , eDNA_13$alpha_detection_method <- c("both eDNA/trawl"), eDNA_13$alpha_detection_method <- c("only eDNA"))
eDNA_13 <- merge(eDNA, eDNA_13, by= c('set_number', 'LCT'))
eDNA_13 <- distinct(eDNA_13)

#let's do this with trawl data 
trawl_13$alpha_detection_method = ifelse(trawl_13$LCT %in% both , trawl_13$alpha_detection_method <- c("both eDNA/trawl"), trawl_13$alpha_detection_method <- c("only trawl"))
trawl_13 <- merge(trawl, trawl_13, by= c('set_number', 'LCT'))
trawl_13 <- distinct(trawl_13)

#set 14
trawl_14 <- trawl[trawl$set_number == 14,]
eDNA_14 <- eDNA[eDNA$set_number == 14,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_14$LCT
eDNA_LCT <- eDNA_14$LCT

botheDNA <- eDNA_14[  eDNA_14$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_14[  trawl_14$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_14[  !(trawl_14$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_14[  !(eDNA_14$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_14$alpha_detection_method = ifelse(eDNA_14$LCT %in% both , eDNA_14$alpha_detection_method <- c("both eDNA/trawl"), eDNA_14$alpha_detection_method <- c("only eDNA"))
eDNA_14 <- merge(eDNA, eDNA_14, by= c('set_number', 'LCT'))
eDNA_14 <- distinct(eDNA_14)

#let's do this with trawl data 
trawl_14$alpha_detection_method = ifelse(trawl_14$LCT %in% both , trawl_14$alpha_detection_method <- c("both eDNA/trawl"), trawl_14$alpha_detection_method <- c("only trawl"))
trawl_14 <- merge(trawl, trawl_14, by= c('set_number', 'LCT'))
trawl_14 <- distinct(trawl_14)

#set 16 

trawl_16 <- trawl[trawl$set_number == 16,]
eDNA_16 <- eDNA[eDNA$set_number == 16,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_16$LCT
eDNA_LCT <- eDNA_16$LCT

botheDNA <- eDNA_16[  eDNA_16$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_16[  trawl_16$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_16[  !(trawl_16$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_16[  !(eDNA_16$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_16$alpha_detection_method = ifelse(eDNA_16$LCT %in% both , eDNA_16$alpha_detection_method <- c("both eDNA/trawl"), eDNA_16$alpha_detection_method <- c("only eDNA"))
eDNA_16 <- merge(eDNA, eDNA_16, by= c('set_number', 'LCT'))
eDNA_16 <- distinct(eDNA_16)

#let's do this with trawl data 
trawl_16$alpha_detection_method = ifelse(trawl_16$LCT %in% both , trawl_16$alpha_detection_method <- c("both eDNA/trawl"), trawl_16$alpha_detection_method <- c("only trawl"))
trawl_16 <- merge(trawl, trawl_16, by= c('set_number', 'LCT'))
trawl_16 <- distinct(trawl_16)


#Merging sets ####
#merge sets 
new_eDNA <- rbind(eDNA_1, eDNA_2, eDNA_3, eDNA_4, eDNA_5, eDNA_7, eDNA_8, eDNA_9, eDNA_10, eDNA_11, eDNA_12, eDNA_13, eDNA_14, eDNA_16)
new_trawl <- rbind(trawl_1, trawl_2, trawl_3, trawl_4, trawl_5, trawl_7, trawl_8, trawl_9, trawl_10, trawl_11, trawl_12, trawl_13, trawl_14, trawl_16)

method_key <- rbind(new_trawl, new_eDNA)
method_key <- distinct(method_key)

method_key <- select(method_key, c('set_number', 'LCT', 'gamma_detection_method.x.x', 'beta_detection_method.x', 'alpha_detection_method'))

method_key <- method_key %>% 
  rename(
    gamma_detection_method = gamma_detection_method.x.x)

method_key <- method_key %>% 
  rename(
    beta_detection_method = beta_detection_method.x)


#add weight + eDNA reads 
#select columns 


eDNA_info <- select(eDNA_df, c('LCT','set_number', 'set_read_index', 'pabs_eDNA'))

p <- merge(method_key, eDNA_info, by=c('LCT','set_number'))
p <- distinct(p)



trawl_info <- select(trawl_df, c('LCT', 'set_number', 'weight_total_kg','pabs_trawl'))

q <- merge(method_key, trawl_info, by=c('LCT', 'set_number'))
q <- distinct(q) 

long <- merge(p, q, by = c("LCT", "set_number", "gamma_detection_method", "alpha_detection_method", "beta_detection_method"), all.x = T, all.y = T) %>% replace(is.na(.), 0)
#env <- merge()

long2 <- long %>%
  group_by(LCT, set_number) %>%
  dplyr::summarise(eDNA_pa = sum(pabs_eDNA)) 

long2 <- distinct(long2)

final <- merge(long, long2, by = c("LCT", "set_number"))

final <- final %>% #rename presence/absence column 
  rename(
    p_abs_eDNA = eDNA_pa)

final <- select(final, -c('pabs_eDNA'))

write_csv(final,
          here("Processed_data",
               "datasets",
               "detections.csv"))

#add environmental data 
trawl_meta <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

#merge final dataset with environmental 
final_env <- merge(final, trawl_meta, by=c('set_number'))

write_csv(final_env,
          here("Processed_data",
               "datasets",
               "detection_env.csv"))

#FOR ALL SETS (1-16) ####
#read in files 
#make eDNA df 
trawl_weight <-  read.csv(here::here("Processed_data",
                                     "trawl",
                                     "datasets",
                                     "trawlweight_allsets.csv"), 
                          head=TRUE)

#make eDNA df 
eDNA_df <-  read.csv(here::here("Processed_data",
                                "eDNA",
                                "datasets",
                                "eDNA_allsets.csv"), #has eDNA index reads
                     head=TRUE)

eDNA_df <- eDNA_df %>% #rename depth column 
  rename(
    depth_eDNA = depth)

eDNA_df <- eDNA_df %>% #rename presence/absence column 
  rename(
    pabs_eDNA = species_pa)

#remove species_pa = NA and index = NA
eDNA_df <- eDNA_df[!is.na(eDNA_df$pabs_eDNA),]
eDNA_df <- eDNA_df[!is.na(eDNA_df$set_read_index),]

#make trawl_df 
trawl_df <-  read.csv(here::here("Processed_data",
                                 "trawl",
                                 "datasets",
                                 "trawl_allsets.csv"), #has weight
                      head=TRUE)

trawl_df <- trawl_df %>% #rename depth column 
  rename(
    depth_trawl = depth_mean)

trawl_df <- cbind(trawl_df, pabs_trawl = 1) #add column with value 1 to indicate presence/absence in trawl df 

trawl_df <- data.frame(lapply(trawl_df, function(x) { #change southern to S across all df
  gsub("southern", "S", x)
  
}))

trawl_df <- data.frame(lapply(trawl_df, function(x) { #change northern to N across all df
  gsub("northern", "N", x)
  
}))


#Selecting relevant columns ####

eDNA <- select(eDNA_df, c('LCT', 'set_number', 'north_south' ))
eDNA <- distinct(eDNA)

trawl <- select(trawl_df, c('LCT', 'set_number', 'north_south'  ))
trawl <- distinct(trawl)

#Gamma Detection ####
#let's start with gamma detection)
#determining gamma error (across whole dataset
#Find overlapping LCT found using both methods 
trawl_LCT <- trawl$LCT
eDNA_LCT <- eDNA$LCT


botheDNA <- eDNA[  eDNA$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl[  trawl$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl[  !(trawl$LCT %in% bothtrawl2), ] #should be less than original trawl df 
onlytrawl <- na.omit(onlytrawl)
#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA[  !(eDNA$LCT %in% botheDNA2), ]  #should have less than original eDNA df 

#Add a new column called method: 
#What I'm going to do is take each individual dataset and add a method that is either trawl,
#or eDNA and then I will merge them together

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA$gamma_detection_method = ifelse(eDNA$LCT %in% both , eDNA$gamma_detection_method <- c("both eDNA/trawl"), eDNA$gamma_detection_method<- c("only eDNA"))

#let's do this with trawl data 
trawl$gamma_detection_method = ifelse(trawl$LCT %in% both , trawl$gamma_detection_method <- c("both eDNA/trawl"), trawl$gamma_detection_method <- c("only trawl"))

#Beta Detection ####

#determining beta error (across regions N/S) 
#northern region
trawl_N <- trawl[trawl$north_south == 'N',]
eDNA_N <- eDNA[eDNA$north_south == 'N',]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_N$LCT
eDNA_LCT <- eDNA_N$LCT

botheDNA <- eDNA_N[  eDNA_N$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_N[  trawl_N$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_N[  !(trawl_N$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_N[  !(eDNA_N$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_N$beta_detection_method = ifelse(eDNA_N$LCT %in% both , eDNA_N$beta_detection_method <- c("both eDNA/trawl"), eDNA_N$beta_detection_method <- c("only eDNA"))
eDNA_N <- merge(eDNA, eDNA_N, by= c('set_number', 'LCT'))
eDNA_N <- distinct(eDNA_N)

#let's do this with trawl data 
trawl_N$beta_detection_method = ifelse(trawl_N$LCT %in% both , trawl_N$beta_detection_method <- c("both eDNA/trawl"), trawl_N$beta_detection_method <- c("only trawl"))
trawl_N <- merge(trawl, trawl_N, by= c('set_number', 'LCT'))
trawl_N <- distinct(trawl_N)

#southern
trawl_S <- trawl[trawl$north_south == 'S',]
eDNA_S <- eDNA[eDNA$north_south == 'S',]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_S$LCT
eDNA_LCT <- eDNA_S$LCT

botheDNA <- eDNA_S[  eDNA_S$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_S[  trawl_S$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_S[  !(trawl_S$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_S[  !(eDNA_S$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_S$beta_detection_method = ifelse(eDNA_S$LCT %in% both , eDNA_S$beta_detection_method <- c("both eDNA/trawl"), eDNA_S$beta_detection_method <- c("only eDNA"))
eDNA_S <- merge(eDNA, eDNA_S, by= c('set_number', 'LCT'))
eDNA_S <- distinct(eDNA_S)

#let's do this with trawl data 
trawl_S$beta_detection_method = ifelse(trawl_S$LCT %in% both , trawl_S$beta_detection_method <- c("both eDNA/trawl"), trawl_S$beta_detection_method <- c("only trawl"))
trawl_S <- merge(trawl, trawl_S, by= c('set_number', 'LCT'))
trawl_S <- distinct(trawl_S)

#merge to eDNA + trawl 
trawl_beta <- rbind(trawl_S,trawl_N)
eDNA_beta <- rbind(eDNA_S, eDNA_N)

#clean up dataset 

eDNA <- select(eDNA_beta, c('LCT', 'set_number', 'north_south.x', 'gamma_detection_method.x', 'beta_detection_method') )
trawl <- select(trawl_beta, c('LCT', 'set_number', 'north_south.x', 'gamma_detection_method.x', 'beta_detection_method') )

#Alpha Detection ####
#the hardest part of this code is going to be adding an set error + gammma error column 
#we can find gamma error from 'method_key_all' 
#set level error can be determined from our beta diversity code (iterative)

#determine beta detection methods
#set 1
trawl_1 <- trawl[trawl$set_number == 1,]
eDNA_1 <- eDNA[eDNA$set_number == 1,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_1$LCT
eDNA_LCT <- eDNA_1$LCT

botheDNA <- eDNA_1[  eDNA_1$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_1[  trawl_1$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_1[  !(trawl_1$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_1[  !(eDNA_1$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_1$alpha_detection_method = ifelse(eDNA_1$LCT %in% both , eDNA_1$alpha_detection_method <- c("both eDNA/trawl"), eDNA_1$alpha_detection_method <- c("only eDNA"))
eDNA_1 <- merge(eDNA, eDNA_1, by= c('set_number', 'LCT'))
eDNA_1 <- distinct(eDNA_1)

#let's do this with trawl data 
trawl_1$alpha_detection_method = ifelse(trawl_1$LCT %in% both , trawl_1$alpha_detection_method <- c("both eDNA/trawl"), trawl_1$alpha_detection_method <- c("only trawl"))
trawl_1 <- merge(trawl, trawl_1, by= c('set_number', 'LCT'))
trawl_1 <- distinct(trawl_1)

#set 2 
trawl_2 <- trawl[trawl$set_number == 2,]
eDNA_2 <- eDNA[eDNA$set_number == 2,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_2$LCT
eDNA_LCT <- eDNA_2$LCT

botheDNA <- eDNA_2[  eDNA_2$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_2[  trawl_2$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_2[  !(trawl_2$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_2[  !(eDNA_2$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_2$alpha_detection_method = ifelse(eDNA_2$LCT %in% both , eDNA_2$alpha_detection_method <- c("both eDNA/trawl"), eDNA_2$alpha_detection_method <- c("only eDNA"))
eDNA_2 <- merge(eDNA, eDNA_2, by= c('set_number', 'LCT'))
eDNA_2 <- distinct(eDNA_2)

#let's do this with trawl data 
trawl_2$alpha_detection_method = ifelse(trawl_2$LCT %in% both , trawl_2$alpha_detection_method <- c("both eDNA/trawl"), trawl_2$alpha_detection_method <- c("only trawl"))
trawl_2 <- merge(trawl, trawl_2, by= c('set_number', 'LCT'))
trawl_2 <- distinct(trawl_2)

#set 3
trawl_3 <- trawl[trawl$set_number == 3,]
eDNA_3 <- eDNA[eDNA$set_number == 3,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_3$LCT
eDNA_LCT <- eDNA_3$LCT

botheDNA <- eDNA_3[  eDNA_3$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_3[  trawl_3$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_3[  !(trawl_3$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_3[  !(eDNA_3$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_3$alpha_detection_method = ifelse(eDNA_3$LCT %in% both , eDNA_3$alpha_detection_method <- c("both eDNA/trawl"), eDNA_3$alpha_detection_method <- c("only eDNA"))
eDNA_3 <- merge(eDNA, eDNA_3, by= c('set_number', 'LCT'))
eDNA_3 <- distinct(eDNA_3)

#let's do this with trawl data 
trawl_3$alpha_detection_method = ifelse(trawl_3$LCT %in% both , trawl_3$alpha_detection_method <- c("both eDNA/trawl"), trawl_3$alpha_detection_method <- c("only trawl"))
trawl_3 <- merge(trawl, trawl_3, by= c('set_number', 'LCT'))
trawl_3 <- distinct(trawl_3)

#set 4
trawl_4 <- trawl[trawl$set_number == 4,]
eDNA_4 <- eDNA[eDNA$set_number == 4,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_4$LCT
eDNA_LCT <- eDNA_4$LCT

botheDNA <- eDNA_4[  eDNA_4$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_4[  trawl_4$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_4[  !(trawl_4$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_4[  !(eDNA_4$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_4$alpha_detection_method = ifelse(eDNA_4$LCT %in% both , eDNA_4$alpha_detection_method <- c("both eDNA/trawl"), eDNA_4$alpha_detection_method <- c("only eDNA"))
eDNA_4 <- merge(eDNA, eDNA_4, by= c('set_number', 'LCT'))
eDNA_4 <- distinct(eDNA_4)

#let's do this with trawl data 
trawl_4$alpha_detection_method = ifelse(trawl_4$LCT %in% both , trawl_4$alpha_detection_method <- c("both eDNA/trawl"), trawl_4$alpha_detection_method <- c("only trawl"))
trawl_4 <- merge(trawl, trawl_4, by= c('set_number', 'LCT'))
trawl_4 <- distinct(trawl_4)


#set 5

trawl_5 <- trawl[trawl$set_number == 5,]
eDNA_5 <- eDNA[eDNA$set_number == 5,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_5$LCT
eDNA_LCT <- eDNA_5$LCT

botheDNA <- eDNA_5[  eDNA_5$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_5[  trawl_5$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_5[  !(trawl_5$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_5[  !(eDNA_5$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_5$alpha_detection_method = ifelse(eDNA_5$LCT %in% both , eDNA_5$alpha_detection_method <- c("both eDNA/trawl"), eDNA_5$alpha_detection_method <- c("only eDNA"))
eDNA_5 <- merge(eDNA, eDNA_5, by= c('set_number', 'LCT'))
eDNA_5 <- distinct(eDNA_5)

#let's do this with trawl data 
trawl_5$alpha_detection_method = ifelse(trawl_5$LCT %in% both , trawl_5$alpha_detection_method <- c("both eDNA/trawl"), trawl_5$alpha_detection_method <- c("only trawl"))
trawl_5 <- merge(trawl, trawl_5, by= c('set_number', 'LCT'))
trawl_5 <- distinct(trawl_5)


#set 6

trawl_6 <- trawl[trawl$set_number == 6,]
eDNA_6 <- eDNA[eDNA$set_number == 6,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_6$LCT
eDNA_LCT <- eDNA_6$LCT

botheDNA <- eDNA_6[  eDNA_6$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_6[  trawl_6$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_6[  !(trawl_6$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_6[  !(eDNA_6$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_6$alpha_detection_method = ifelse(eDNA_6$LCT %in% both , eDNA_6$alpha_detection_method <- c("both eDNA/trawl"), eDNA_6$alpha_detection_method <- c("only eDNA"))
eDNA_6 <- merge(eDNA, eDNA_6, by= c('set_number', 'LCT'))
eDNA_6 <- distinct(eDNA_6)

#let's do this with trawl data 
trawl_6$alpha_detection_method = ifelse(trawl_6$LCT %in% both , trawl_6$alpha_detection_method <- c("both eDNA/trawl"), trawl_6$alpha_detection_method <- c("only trawl"))
trawl_6 <- merge(trawl, trawl_6, by= c('set_number', 'LCT'))
trawl_6 <- distinct(trawl_6)

#set 7 
trawl_7 <- trawl[trawl$set_number == 7,]
eDNA_7 <- eDNA[eDNA$set_number == 7,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_7$LCT
eDNA_LCT <- eDNA_7$LCT

botheDNA <- eDNA_7[  eDNA_7$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_7[  trawl_7$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_7[  !(trawl_7$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_7[  !(eDNA_7$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_7$alpha_detection_method = ifelse(eDNA_7$LCT %in% both , eDNA_7$alpha_detection_method <- c("both eDNA/trawl"), eDNA_7$alpha_detection_method <- c("only eDNA"))
eDNA_7 <- merge(eDNA, eDNA_7, by= c('set_number', 'LCT'))
eDNA_7 <- distinct(eDNA_7)

#let's do this with trawl data 
trawl_7$alpha_detection_method = ifelse(trawl_7$LCT %in% both , trawl_7$alpha_detection_method <- c("both eDNA/trawl"), trawl_7$alpha_detection_method <- c("only trawl"))
trawl_7 <- merge(trawl, trawl_7, by= c('set_number', 'LCT'))
trawl_7 <- distinct(trawl_7)

#set 8 
trawl_8 <- trawl[trawl$set_number == 8,]
eDNA_8 <- eDNA[eDNA$set_number == 8,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_8$LCT
eDNA_LCT <- eDNA_8$LCT

botheDNA <- eDNA_8[  eDNA_8$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_8[  trawl_8$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_8[  !(trawl_8$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_8[  !(eDNA_8$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_8$alpha_detection_method = ifelse(eDNA_8$LCT %in% both , eDNA_8$alpha_detection_method <- c("both eDNA/trawl"), eDNA_8$alpha_detection_method <- c("only eDNA"))
eDNA_8 <- merge(eDNA, eDNA_8, by= c('set_number', 'LCT'))
eDNA_8 <- distinct(eDNA_8)

#let's do this with trawl data 
trawl_8$alpha_detection_method = ifelse(trawl_8$LCT %in% both , trawl_8$alpha_detection_method <- c("both eDNA/trawl"), trawl_8$alpha_detection_method <- c("only trawl"))
trawl_8 <- merge(trawl, trawl_8, by= c('set_number', 'LCT'))
trawl_8 <- distinct(trawl_8)

#set 9 
trawl_9 <- trawl[trawl$set_number == 9,]
eDNA_9 <- eDNA[eDNA$set_number == 9,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_9$LCT
eDNA_LCT <- eDNA_9$LCT

botheDNA <- eDNA_9[  eDNA_9$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_9[  trawl_9$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_9[  !(trawl_9$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_9[  !(eDNA_9$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_9$alpha_detection_method = ifelse(eDNA_9$LCT %in% both , eDNA_9$alpha_detection_method <- c("both eDNA/trawl"), eDNA_9$alpha_detection_method <- c("only eDNA"))
eDNA_9 <- merge(eDNA, eDNA_9, by= c('set_number', 'LCT'))
eDNA_9 <- distinct(eDNA_9)

#let's do this with trawl data 
trawl_9$alpha_detection_method = ifelse(trawl_9$LCT %in% both , trawl_9$alpha_detection_method <- c("both eDNA/trawl"), trawl_9$alpha_detection_method <- c("only trawl"))
trawl_9 <- merge(trawl, trawl_9, by= c('set_number', 'LCT'))
trawl_9 <- distinct(trawl_9)

#set 10 
trawl_10 <- trawl[trawl$set_number == 10,]
eDNA_10 <- eDNA[eDNA$set_number == 10,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_10$LCT
eDNA_LCT <- eDNA_10$LCT

botheDNA <- eDNA_10[  eDNA_10$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_10[  trawl_10$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_10[  !(trawl_10$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_10[  !(eDNA_10$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_10$alpha_detection_method = ifelse(eDNA_10$LCT %in% both , eDNA_10$alpha_detection_method <- c("both eDNA/trawl"), eDNA_10$alpha_detection_method <- c("only eDNA"))
eDNA_10 <- merge(eDNA, eDNA_10, by= c('set_number', 'LCT'))
eDNA_10 <- distinct(eDNA_10)

#let's do this with trawl data 
trawl_10$alpha_detection_method = ifelse(trawl_10$LCT %in% both , trawl_10$alpha_detection_method <- c("both eDNA/trawl"), trawl_10$alpha_detection_method <- c("only trawl"))
trawl_10 <- merge(trawl, trawl_10, by= c('set_number', 'LCT'))
trawl_10 <- distinct(trawl_10)

#set 11
trawl_11 <- trawl[trawl$set_number == 11,]
eDNA_11 <- eDNA[eDNA$set_number == 11,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_11$LCT
eDNA_LCT <- eDNA_11$LCT

botheDNA <- eDNA_11[  eDNA_11$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_11[  trawl_11$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_11[  !(trawl_11$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_11[  !(eDNA_11$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_11$alpha_detection_method = ifelse(eDNA_11$LCT %in% both , eDNA_11$alpha_detection_method <- c("both eDNA/trawl"), eDNA_11$alpha_detection_method <- c("only eDNA"))
eDNA_11 <- merge(eDNA, eDNA_11, by= c('set_number', 'LCT'))
eDNA_11 <- distinct(eDNA_1)

#let's do this with trawl data 
trawl_11$alpha_detection_method = ifelse(trawl_11$LCT %in% both , trawl_11$alpha_detection_method <- c("both eDNA/trawl"), trawl_11$alpha_detection_method <- c("only trawl"))
trawl_11 <- merge(trawl, trawl_11, by= c('set_number', 'LCT'))
trawl_11 <- distinct(trawl_11)

#set 12
trawl_12 <- trawl[trawl$set_number == 12,]
eDNA_12 <- eDNA[eDNA$set_number == 12,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_12$LCT
eDNA_LCT <- eDNA_12$LCT

botheDNA <- eDNA_12[  eDNA_12$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_12[  trawl_12$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_12[  !(trawl_12$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_12[  !(eDNA_12$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_12$alpha_detection_method = ifelse(eDNA_12$LCT %in% both , eDNA_12$alpha_detection_method <- c("both eDNA/trawl"), eDNA_12$alpha_detection_method <- c("only eDNA"))
eDNA_12 <- merge(eDNA, eDNA_12, by= c('set_number', 'LCT'))
eDNA_12 <- distinct(eDNA_12)

#let's do this with trawl data 
trawl_12$alpha_detection_method = ifelse(trawl_12$LCT %in% both , trawl_12$alpha_detection_method <- c("both eDNA/trawl"), trawl_12$alpha_detection_method <- c("only trawl"))
trawl_12 <- merge(trawl, trawl_12, by= c('set_number', 'LCT'))
trawl_12 <- distinct(trawl_12)

#set 13
trawl_13 <- trawl[trawl$set_number == 13,]
eDNA_13 <- eDNA[eDNA$set_number == 13,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_13$LCT
eDNA_LCT <- eDNA_13$LCT

botheDNA <- eDNA_13[  eDNA_13$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_13[  trawl_13$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_13[  !(trawl_13$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_13[  !(eDNA_13$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_13$alpha_detection_method = ifelse(eDNA_13$LCT %in% both , eDNA_13$alpha_detection_method <- c("both eDNA/trawl"), eDNA_13$alpha_detection_method <- c("only eDNA"))
eDNA_13 <- merge(eDNA, eDNA_13, by= c('set_number', 'LCT'))
eDNA_13 <- distinct(eDNA_13)

#let's do this with trawl data 
trawl_13$alpha_detection_method = ifelse(trawl_13$LCT %in% both , trawl_13$alpha_detection_method <- c("both eDNA/trawl"), trawl_13$alpha_detection_method <- c("only trawl"))
trawl_13 <- merge(trawl, trawl_13, by= c('set_number', 'LCT'))
trawl_13 <- distinct(trawl_13)

#set 14
trawl_14 <- trawl[trawl$set_number == 14,]
eDNA_14 <- eDNA[eDNA$set_number == 14,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_14$LCT
eDNA_LCT <- eDNA_14$LCT

botheDNA <- eDNA_14[  eDNA_14$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_14[  trawl_14$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_14[  !(trawl_14$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_14[  !(eDNA_14$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_14$alpha_detection_method = ifelse(eDNA_14$LCT %in% both , eDNA_14$alpha_detection_method <- c("both eDNA/trawl"), eDNA_14$alpha_detection_method <- c("only eDNA"))
eDNA_14 <- merge(eDNA, eDNA_14, by= c('set_number', 'LCT'))
eDNA_14 <- distinct(eDNA_14)

#let's do this with trawl data 
trawl_14$alpha_detection_method = ifelse(trawl_14$LCT %in% both , trawl_14$alpha_detection_method <- c("both eDNA/trawl"), trawl_14$alpha_detection_method <- c("only trawl"))
trawl_14 <- merge(trawl, trawl_14, by= c('set_number', 'LCT'))
trawl_14 <- distinct(trawl_14)


#set 15

trawl_15 <- trawl[trawl$set_number == 15,]
eDNA_15 <- eDNA[eDNA$set_number == 15,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_15$LCT
eDNA_LCT <- eDNA_15$LCT

botheDNA <- eDNA_15[  eDNA_15$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_15[  trawl_15$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_15[  !(trawl_15$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_15[  !(eDNA_15$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_15$alpha_detection_method = ifelse(eDNA_15$LCT %in% both , eDNA_15$alpha_detection_method <- c("both eDNA/trawl"), eDNA_15$alpha_detection_method <- c("only eDNA"))
eDNA_15 <- merge(eDNA, eDNA_15, by= c('set_number', 'LCT'))
eDNA_15 <- distinct(eDNA_15)

#let's do this with trawl data 
trawl_15$alpha_detection_method = ifelse(trawl_15$LCT %in% both , trawl_15$alpha_detection_method <- c("both eDNA/trawl"), trawl_15$alpha_detection_method <- c("only trawl"))
trawl_15 <- merge(trawl, trawl_15, by= c('set_number', 'LCT'))
trawl_15 <- distinct(trawl_15)

#set 16 

trawl_16 <- trawl[trawl$set_number == 16,]
eDNA_16 <- eDNA[eDNA$set_number == 16,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_16$LCT
eDNA_LCT <- eDNA_16$LCT

botheDNA <- eDNA_16[  eDNA_16$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_16[  trawl_16$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_16[  !(trawl_16$LCT %in% bothtrawl2), ] #

#we need to find LCT only found in eDNA 

botheDNA2 <- botheDNA$LCT

onlyeDNA <- eDNA_16[  !(eDNA_16$LCT %in% botheDNA2), ]  

#Add a new column called method: 

onlytrawl = onlytrawl$LCT
onlyeDNA = onlyeDNA$LCT
both = both$LCT

#let's do this with eDNA data 
eDNA_16$alpha_detection_method = ifelse(eDNA_16$LCT %in% both , eDNA_16$alpha_detection_method <- c("both eDNA/trawl"), eDNA_16$alpha_detection_method <- c("only eDNA"))
eDNA_16 <- merge(eDNA, eDNA_16, by= c('set_number', 'LCT'))
eDNA_16 <- distinct(eDNA_16)

#let's do this with trawl data 
trawl_16$alpha_detection_method = ifelse(trawl_16$LCT %in% both , trawl_16$alpha_detection_method <- c("both eDNA/trawl"), trawl_16$alpha_detection_method <- c("only trawl"))
trawl_16 <- merge(trawl, trawl_16, by= c('set_number', 'LCT'))
trawl_16 <- distinct(trawl_16)


#Merging sets ####
#merge sets 
new_eDNA <- rbind(eDNA_1, eDNA_2, eDNA_3, eDNA_4, eDNA_5, eDNA_6, eDNA_7, eDNA_8, eDNA_9, eDNA_10, eDNA_11, eDNA_12, eDNA_13, eDNA_14,eDNA_15, eDNA_16)
new_trawl <- rbind(trawl_1, trawl_2, trawl_3, trawl_4, trawl_5,trawl_6, trawl_7, trawl_8, trawl_9, trawl_10, trawl_11, trawl_12, trawl_13, trawl_14, trawl_15, trawl_16)

method_key <- rbind(new_trawl, new_eDNA)
method_key <- distinct(method_key)

method_key <- select(method_key, c('set_number', 'LCT', 'gamma_detection_method.x.x', 'beta_detection_method.x', 'alpha_detection_method'))

method_key <- method_key %>% 
  rename(
    gamma_detection_method = gamma_detection_method.x.x)

method_key <- method_key %>% 
  rename(
    beta_detection_method = beta_detection_method.x)


#add weight + eDNA reads 
#select columns 

eDNA_info <- select(eDNA_df, c('LCT','set_number', 'set_read_index', 'pabs_eDNA'))

p <- merge(method_key, eDNA_info, by=c('LCT','set_number'))
p <- distinct(p)


trawl_weight$pabs_trawl <- 1
trawl_info <- select(trawl_weight, c('LCT', 'set_number', 'weight_total_kg','pabs_trawl'))

q <- merge(method_key, trawl_info, by=c('LCT', 'set_number'))
q <- distinct(q) 

long <- merge(p, q, by = c("LCT", "set_number", "gamma_detection_method", "alpha_detection_method", "beta_detection_method"), all.x = T, all.y = T) %>% replace(is.na(.), 0)

#env <- merge()

long2 <- long %>%
  group_by(LCT, set_number) %>%
  dplyr::summarise(eDNA_pa = sum(pabs_eDNA)) 

long2 <- distinct(long2)

final <- merge(long, long2, by = c("LCT", "set_number"))

final <- final %>% #rename presence/absence column 
  rename(
    p_abs_eDNA = eDNA_pa)

final <- select(final, -c('pabs_eDNA'))

write_csv(final,
          here("Processed_data",
               "datasets",
               "detections_all.csv"))

#add environmental data 
trawl_meta <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

#merge final dataset with environmental 
final_env <- merge(final, trawl_meta, by=c('set_number'))

write_csv(final_env,
          here("Processed_data",
               "datasets",
               "detection_all_env.csv"))
