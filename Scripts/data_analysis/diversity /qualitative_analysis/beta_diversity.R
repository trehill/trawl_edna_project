#Explore Gamma (N/S) Diversity 
#Set-Up ####

library(eulerr)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(RColorBrewer)
library(dplyr)

#SET 1 ####

trawl_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  "fulldatasettrawlmin.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "eDNAfulldatasetmin.csv"),
                      head=TRUE)

#subset by set 1

trawl_data <- trawl_data[trawl_data$set_number == 1,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 1,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))

#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 2 ####
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "fulldatasettrawlmin.csv"),
                       head=TRUE)

eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "eDNAfulldatasetmin.csv"),
                      head=TRUE)

#subset by set 2

trawl_data <- trawl_data[trawl_data$set_number == 2,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 2,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))

#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 3 ####
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

#subset by set 3

trawl_data <- trawl_data[trawl_data$set_number == 3,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 3,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))

#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 4 ####
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

#subset by set 4

trawl_data <- trawl_data[trawl_data$set_number == 4,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 4,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 5 ####
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

#subset by set 5

trawl_data <- trawl_data[trawl_data$set_number == 5,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 5,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))

#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)


#SET 7 ####
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

#subset by set 7

trawl_data <- trawl_data[trawl_data$set_number == 7,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 7,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 8 ####
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

#subset by set 8

trawl_data <- trawl_data[trawl_data$set_number == 8,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 8,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))

#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 9 ####
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

#subset by set 9

trawl_data <- trawl_data[trawl_data$set_number == 9,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 9,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 10 ####
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

#subset by set 10

trawl_data <- trawl_data[trawl_data$set_number == 10,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 10,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 11 ####
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

#subset by set 11

trawl_data <- trawl_data[trawl_data$set_number == 11,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 11,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 12 ####
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

#subset by set 12

trawl_data <- trawl_data[trawl_data$set_number == 12,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 12,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 13 ####
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

#subset by set 13

trawl_data <- trawl_data[trawl_data$set_number == 13,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 13,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 14 ####
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

#subset by set 14

trawl_data <- trawl_data[trawl_data$set_number == 14,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 14,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))

#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#SET 16 ####

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

#subset by set 16

trawl_data <- trawl_data[trawl_data$set_number == 16,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 16,]

#Find overlapping LCT found using both methods 
trawl_LCT <- trawl_data$LCT
eDNA_LCT <- eDNA_data$LCT

botheDNA <- eDNA_data[  eDNA_data$LCT %in% trawl_LCT, ] 
#check that it goes both ways..?
bothtrawl <- trawl_data[  trawl_data$LCT %in% eDNA_LCT, ] 

#check that they are the same LCT
unique(botheDNA$LCT) 
unique(bothtrawl$LCT) #yay they are the same! 

both = bothtrawl
#we need to find LCT only found in trawl 
bothtrawl2 <- bothtrawl$LCT

onlytrawl <- trawl_data[  !(trawl_data$LCT %in% bothtrawl2), ] #should be less than original trawl df 

#we need to find LCT only found in eDNA 

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

plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4), quantities=TRUE )

plot(fit, quantities = list(cex=4), fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))


#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

