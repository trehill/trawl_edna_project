
#Set-Up ####
library(eulerr)
library(ggplot2)
library(rphylopic)
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


data2 <- data.frame(lapply(data, function(x) { #change southern to S across all df
  gsub("both eDNA/trawl", "both", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only trawl", "trawl", x)
  
}))

data2 <- data.frame(lapply(data2, function(x) { #change southern to S across all df
  gsub("only eDNA", "eDNA", x)
  
}))

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

alosa_plot <- plot(euler(data_new), legend = TRUE)

alosa_plot

#lets try to add pictures! 

alosa <- name_search(text = "Alosa sapidissima", options = "namebankID")[[1]] # find names
alosa

alosa_id_all <- name_images(uuid = alosa$uid[1])  # list images
alosa_id_all

alosa_id <- name_images(uuid = alosa$uid[1])$same[[1]]$uid  # get individual image id
alosa_id

#add points as fish 

uuid <- alosa_id
img <- image_data(uuid, size = "64")[[1]]
(p <- ggplot(mtcars, aes(drat, wt)) + geom_blank())
for(i in 1:nrow(mtcars)) p <- p + add_phylopic(img, 1, mtcars$drat[i], mtcars$wt[i], ysize = 0.3)
p

#let's try to add image as venn elements 

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
p <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", 
               fill_color = brewer.pal(name="Set2",n=3), text_size = 2) + geom_blank()
for(i in 1:nrow(df)) p <- p + add_phylopic(img, 1, df$value[i], df$value[i], ysize = 0.3)
p
ggvenn(df)

#does not seem to work with ggvenn so lets try w/ eulerr 


data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "beta_div.csv"),
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


uuid <- alosa_id
img <- image_data(uuid, size = "64")[[1]]
(p <- ggplot(mtcars, aes(drat, wt)) + geom_blank())
for(i in 1:nrow(mtcars)) p <- p + add_phylopic(img, 1, mtcars$drat[i], mtcars$wt[i], ysize = 0.3)
p

(p <- plot(euler(data_new), legend = TRUE) + geom_blank())
