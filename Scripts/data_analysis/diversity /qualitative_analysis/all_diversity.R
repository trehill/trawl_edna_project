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


data <- read.csv(here::here("Processed_data", 
                                "datasets",
                                  "beta_div.csv"),
                       head=TRUE)

data2 <- select(data, c('LCT', 'gamma_detection_method'))
data2 <- distinct(data2)

#plot overlap of methods throughout the ENTIRE dataset 
#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(data2$gamma_detection_method==c("only trawl")))
b <- length(which(data2$gamma_detection_method==c("only eDNA")))
ab <- length(which(data2$gamma_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/gamma_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)


#save png 
ggsave("./Outputs/diversity/gamma_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#OLD CODE ####

trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data", #should be ASV by sample
                                 "eDNA",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)



#Creation of 3 datasets (trawl only, eDNA only, both) ####

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
onlytrawl <- na.omit(onlytrawl)
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


write_csv(data,
          here("Processed_data",
               "euler",
               "method_key_all.csv"))


#Okay, now we have our data! 
data <- distinct(data)

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(data$method==c("trawl")))
b <- length(which(data$method==c("eDNA")))
ab <- length(which(data$method==c("both")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))

#save png 

#Using ggVenn

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
ggvenn(df)

#save png 

