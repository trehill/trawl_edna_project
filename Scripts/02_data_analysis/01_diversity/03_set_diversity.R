#Explore Alpha Diversity 
#Set-Up ####

library(eulerr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(here)
library(RColorBrewer)
library(dplyr)


#SET-UP ####
data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections.csv"),
                 head=TRUE)

data2 <- select(data, c('LCT', 'alpha_detection_method', 'set_number'))
data2 <- distinct(data2)


#EXCLUDED SETS ####

#format data 

data_long <- select(data, c('LCT', 'set_number', 'alpha_detection_method'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, alpha_detection_method, var)
data_wide[is.na(data_wide)] <- FALSE  #replace NA with FALSE 
data_new <- select(data_wide, -c('LCT'))

data_new$set_number <- as.character(data_new$set_number)

#this kind of worked but we want them to overlap 
data_long <- select(data, c('LCT', 'set_number', 'pabs_trawl', 'p_abs_eDNA'))

#we want to change all p_abs_trawl/edna 1 = true, 0 = false
data_long$pabs_trawl<-ifelse(data_long$pabs_trawl=="1",TRUE,FALSE)
data_long$p_abs_eDNA<-ifelse(data_long$p_abs_eDNA=="1",TRUE,FALSE)

#rename pabs variables 
data_long <- data_long %>% 
  rename(
    trawl = pabs_trawl,
    eDNA = p_abs_eDNA
  )

data_long <- select(data_long, -c('LCT'))

#arrange in ascending set_number 
data_long <- data_long %>% arrange(as.numeric(set_number)) 

data_long$set_number <- as.character(data_long$set_number)

plot <- plot(euler(data_long, by = list(set_number)), legend = TRUE, fills = c("#fef3da", "#ebf9fa","#dbe8f5"))
plot 

ggsave("./Outputs/diversity/alpha_eulerr.png", 
       plot = plot,
       width = 12, height = 2, units = "in")

#ALL SETS ####


data <- read.csv(here::here("Processed_data", 
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

data2 <- select(data, c('LCT', 'alpha_detection_method', 'set_number'))
data2 <- distinct(data2)



#format data 

data_long <- select(data, c('LCT', 'set_number', 'alpha_detection_method'))
data_long$var <- TRUE #add 'true' column
data_wide <- spread(data_long, alpha_detection_method, var)
data_wide[is.na(data_wide)] <- FALSE  #replace NA with FALSE 
data_new <- select(data_wide, -c('LCT'))

data_new$set_number <- as.character(data_new$set_number)

#this kind of worked but we want them to overlap 
data_long <- select(data, c('LCT', 'set_number', 'pabs_trawl', 'p_abs_eDNA'))

#we want to change all p_abs_trawl/edna 1 = true, 0 = false
data_long$pabs_trawl<-ifelse(data_long$pabs_trawl=="1",TRUE,FALSE)
data_long$p_abs_eDNA<-ifelse(data_long$p_abs_eDNA=="1",TRUE,FALSE)

#rename pabs variables 
data_long <- data_long %>% 
  rename(
    trawl = pabs_trawl,
    eDNA = p_abs_eDNA
  )

data_long <- select(data_long, -c('LCT'))

#arrange in ascending set_number 
data_long <- data_long %>% arrange(as.numeric(set_number)) 

data_long$set_number <- as.character(data_long$set_number)

plot <- plot(euler(data_long, by = list(set_number)), legend = TRUE, fills = c("#ebf9fa","#fef3da","#dbe8f5"))
plot 

ggsave("./Outputs/diversity/alpha_eulerr_all.png", 
       plot = plot,
       width = 12, height = 2, units = "in")

#OLD CODE ####

#repeat the same code for each set 
#SET 1 #### 

set_data <- data2[data2$set_number == 1,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right", main='Set 1'))
plot 

#save png 
ggsave("./Outputs/diversity/set1_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 1,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 1,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set1_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")


#SET 2 ####

set_data <- data2[data2$set_number == 2,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set2_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 2,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 2,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set2_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 3 ####

set_data <- data2[data2$set_number == 3,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set3_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 3,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 3,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot

#save png 
ggsave("./Outputs/diversity/set3_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 4 ####
set_data <- data2[data2$set_number == 4,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set4_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 4,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 4,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set4_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 5 ####
set_data <- data2[data2$set_number == 5,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set5_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 5,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 5,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set5_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 7 ####
set_data <- data2[data2$set_number == 7,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set7_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 7,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 7,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set7_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")


#SET 8 ####
set_data <- data2[data2$set_number == 8,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set8_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 8,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 8,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set8_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 9 ####
set_data <- data2[data2$set_number == 9,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set9_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 9,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 9,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set9_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 10 ####
set_data <- data2[data2$set_number == 10,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set10_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 10,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 10,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set10_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")


#SET 11 ####
set_data <- data2[data2$set_number == 11,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set11_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 11,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 11,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set11_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 12 ####
set_data <- data2[data2$set_number == 12,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set12_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 12,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 12,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set12_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 13 ####
set_data <- data2[data2$set_number == 13,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set13_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 13,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 13,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set13_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 14 ####
set_data <- data2[data2$set_number == 14,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set14_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 14,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 14,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set14_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#SET 16 ####
set_data <- data2[data2$set_number == 16,]

#Using Eulerr 
#A = trawl (only trawl)
#B = eDNA  (only eDNA)
#A&B = both 

a <- length(which(set_data$alpha_detection_method==c("only trawl")))
b <- length(which(set_data$alpha_detection_method==c("only eDNA")))
ab <- length(which(set_data$alpha_detection_method==c("both eDNA/trawl")))

fit <- euler(c("A" = a, "B" = b, "A&B" = ab))

plot <- plot(fit, quantities = TRUE, fills = list(fill = c("orangered", "royalblue1"), alpha=0.5), legend = list(labels = c("trawl", "eDNA"), side="right"))
plot 

#save png 
ggsave("./Outputs/diversity/set16_euler.png", 
       plot = plot,
       width = 2, height = 2, units = "in")

#Using GGVENN
trawl_data <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "datasets",
                                  "fulldatasettrawl.csv"),
                       head=TRUE)


eDNA_data <- read.csv(here::here("Processed_data",  
                                 "eDNA",
                                 "datasets",
                                 "eDNAfulldataset.csv"),
                      head=TRUE)


trawl_data <- trawl_data[trawl_data$set_number == 16,]
eDNA_data <- eDNA_data[eDNA_data$set_number == 16,]

df <- list(`Trawl` = c(trawl_data$LCT),
           `eDNA` = c(eDNA_data$LCT))

ggvenn(df, c("Trawl", "eDNA"))
plot <- ggvenn(df,c("Trawl", "eDNA"), show_elements = T, label_sep = "\n", fill_color = brewer.pal(name="Set2",n=3), text_size = 2)
plot


#save png 
ggsave("./Outputs/diversity/set16_venn.png", 
       plot = plot,
       width = 2, height = 2, units = "in")
     