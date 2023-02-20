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
                                  "detections.csv"),
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

#NEW CODE ###
#plot 
data_long <- select(data2, c('LCT','set_number', 'gamma_detection_method'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, gamma_detection_method, var)
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
data_new <- select(data_new, c('eDNA', 'trawl'))

#switch TRUE + FALSE 

plot <- plot(euler(data_new), legend = TRUE)

plot

#save plot
ggsave("./Outputs/diversity/gamma_diversity.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


#OLD CODE ####
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
ggsave("./Outputs/diversity/all_euler.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

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
ggvenn(df) #check the same as euler 
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot
ggvenn(df)

#save png 
ggsave("./Outputs/diversity/all_venn.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


