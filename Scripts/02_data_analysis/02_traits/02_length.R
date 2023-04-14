#Analysis of Length Distributions Across Methods 
#Author: Tessa Rehill 

#Set-Up ####
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)
library(forcats)

#set-up, read in files ####

beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "detections_all.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)


trawl_catch  <- read.csv(here::here("Processed_data", 
                                    "traits",
                                    "length_conversions.csv"),
                         head=TRUE)


#Formatting data for plotting ####

#merge detection data w/ trait data 
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)

#change 'both' category to eDNA or trawl alone
#subset for 'both' data only 
both_to_eDNA <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to eDNA 
both_to_eDNA <- data.frame(lapply(both_to_eDNA, function(x) {
  gsub("both eDNA/trawl", "eDNA", x) }))

#same for trawl 
both_to_trawl <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to trawl 
both_to_trawl <- data.frame(lapply(both_to_trawl, function(x) {
  gsub("both eDNA/trawl", "trawl", x) 
  
}))

#merge together 
both_fixed <- rbind(both_to_eDNA, both_to_trawl)

#merge back with our original data
without_both <- subset(data, gamma_detection_method != 'both eDNA/trawl')
data_new <- rbind(both_fixed, without_both)

#fix only... to simple trawl or eDNA 
data <- data.frame(lapply(data_new, function(x) {
  gsub("only eDNA", "eDNA", x) }))

data <- data.frame(lapply(data, function(x) {
  gsub("only trawl", "trawl", x) }))


#let's plot!

#Individual Length Distributions ####
#check out individual length distribution of fish caught in trawl 
trawl_catch <- trawl_catch[!is.na(trawl_catch$total_length_cm),] #remove NA


plot <- ggplot(trawl_catch, aes(x=total_length_cm))+
  geom_histogram(color="#5491cf", fill="#5491cf", binwidth = 2,)+
  theme_classic()

plot

ggsave("./Outputs/traits/ind_length_his.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#Species Length Distributions ####

#plotting species traits (not individual traits)
speciestraits= subset(data, select = c(LCT, max_length_cm, gamma_detection_method) ) #select relevants columns
speciestraits <- distinct(speciestraits) #ensure no replicates
speciestraits <- speciestraits %>% drop_na(max_length_cm) #ensure no NA values
speciestraits$max_length_cm <- as.numeric(speciestraits$max_length_cm)  #make values numeric (instead of character)

#plot species length distribution for all trawl + all eDNA (does not differentiate 'both')

plot <- speciestraits %>%
  ggplot(aes(y = gamma_detection_method)) +
  geom_density_ridges(
    aes(x = max_length_cm , fill = gamma_detection_method), 
    alpha = 1, color = "white", from = 0, to = 250,
    bandwidth=18) +
  labs(
    x = "maximum species length (cm)",
    y = '') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c("#FCC442", "#5491cf"),
    name = "detection", guide = "legend"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE)


plot 

ggsave("./Outputs/traits/trawleDNA_density.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#Species + Individual Length Distributions ####
#goal: make a density plot like above with four rows 
#trawl only max species length 
#eDNA only max species length 
#trawl + eDNA max species length 
#trawl only individual length 

#merge original data w/ traits 
data2 <- merge(beta_div, trait_data, by="LCT", all.x= TRUE) #merge original data w/ traits 
data2 <- select(data2, c('LCT','gamma_detection_method', 'max_length_cm'))#extract relevant columns 
colnames(data2) <- c('LCT', 'detection', 'length_cm') #change column names
catch <- select(trawl_catch, c('species', 'total_length_cm')) #select relevant columns
colnames(catch) <- c('LCT','length_cm') #change column names

#add column detection = trawl individuals
catch$detection <- c('trawl individuals')

#merge all data
length <- rbind(catch, data2) #this includes non-fish... should we get rid of them? 

write_csv(length,
          here("Processed_data",
               "traits",
               "length.csv")) 

#plot
length <- distinct(length) # we only want one observation per species 

plot <- ggplot(length, 
               aes(x = length_cm, 
                   y = detection, 
                   fill = detection)) +
  geom_density_ridges(bandwidth=8) + 
  theme_ridges() +
  labs("") +
  xlab("length (cm)") + ylab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#00AFBB","#FCC442", "#5491cf","#b9d2eb")) +
  theme_classic()
plot 


ggsave("./Outputs/traits/all_length_dist.png", 
       plot = plot,
       width = 10, height = 6, units = "in")


#Species Length Distributions w/ 'Both' Group
#plot density but differentiate by groups 
#we need to combine speciestraits to beta_div 

method_2 <- select(beta_div, c('LCT','gamma_detection_method')) #select relevant column 

#rename species traits columns 
speciestraits <- speciestraits %>%  #rename column 
  rename(method = gamma_detection_method)


data <- merge(speciestraits, method_2, by="LCT", all.x= TRUE)

data <- distinct(data)

plot <- data %>%
  ggplot(aes(y = method)) +
  geom_density_ridges(
    aes(x = max_length_cm , fill = gamma_detection_method), 
    alpha = 1, color = "white", from = 0, to = 250,
    bandwidth=18) +
  labs(
    x = "maximum species length (cm)",
    y = '') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c("#00AFBB","#FCC442", "#5491cf"),
    name = "detection", guide = "legend"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE)

plot


ggsave("./Outputs/traits/trawl_edna_2.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#Stacked distributions 

#cut = trawl or eDNA 
data2 <- data 
colnames(data2) <- c("LCT", "max_length_cm", "method","detection") #change column names
data2 <- distinct(data2)

plot <- ggplot(data2, aes(max_length_cm)) +
  geom_density(aes(fill = detection), position = "stack", bw=12) + 
  scale_fill_manual(values=c("#00AFBB","#FCC442", "#5491cf"))+
  scale_y_discrete(expand = c(0, 0)) +
  theme_classic() + 
  labs(
    x = "maximum species length (cm)") +
  facet_wrap(~method) 

plot

ggsave("./Outputs/traits/length_bymethod_stack.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


#Species Length Distributions (all on same line)
data$method <- c('detection')
data <- distinct(data)

plot <- data %>%
  ggplot(aes(y = method)) +
  geom_density_ridges(
    aes(x = max_length_cm , fill = gamma_detection_method), 
    alpha = 1, color = "white", from = 0, to = 250,
    bandwidth=18) +
  labs(
    x = "maximum species length (cm)",
    y = '') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c("#00AFBB","#FCC442", "#5491cf"),
    name = "detection", guide = "legend"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE)

plot


ggsave("./Outputs/traits/max_length_density.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#Determining mean length of Eulachon ####
eulachon <- subset(trawl_catch, species == c("Thaleichthys pacificus"))
mean(eulachon$length_cm)




