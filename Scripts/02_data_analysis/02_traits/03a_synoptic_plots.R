#Plotting synoptic lengths 
#goal: plot length distributions of synoptic lengths 

#PLOT ####
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)

#set-up, read in files 
trait_data <- read.csv(here::here("Processed_data", 
                                  "datasets",
                                  "traits",
                                  "traits_mean_lengths.csv"), #original trait database w/ added mean lengths 
                       head=TRUE)


detection <- read.csv(here::here("Processed_data", 
                                "datasets",
                                "detections_all.csv"),
                     head=TRUE)

trawl_catch  <- read.csv(here::here("Processed_data", 
                                    "datasets",
                                    "traits",
                                    "length_conversions.csv"),
                         head=TRUE) #fork to total length conversion for indv. trawl spp. 



#Formatting data for plotting ####

#merge detection data w/ trait data (mean lengths)
data <- merge(detection, trait_data, by="LCT", all.x= TRUE)

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
  gsub("both eDNA/trawl", "trawl", x) }))

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


#Let's plot! ####
#Species Length Distributions 
#plotting species traits (not individual traits)
speciestraits= subset(data, select = c(LCT, total_mean_length_cm, gamma_detection_method) ) #select relevants columns
speciestraits <- distinct(speciestraits) #ensure no replicates
speciestraits <- speciestraits %>% drop_na(total_mean_length_cm) #ensure no NA values
speciestraits$total_mean_length_cm <- as.numeric(speciestraits$total_mean_length_cm)  #make values numeric (instead of character)

#plot species length distribution for all trawl + all eDNA (does not differentiate 'both')

plot <- speciestraits %>%
  ggplot(aes(y = gamma_detection_method)) +
  geom_density_ridges(
    aes(x = total_mean_length_cm , fill = gamma_detection_method), 
    alpha = 1, color = "white", from = 0, to = 150,
    bandwidth=13) +
  labs(
    x = "mean species length (cm)",
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

ggsave("./Outputs/traits/syn_length/synop_eDNAtrawl_density.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#Species + Individual Length Distributions 
#goal: make a density plot like above with four rows 
#trawl only max species length 
#eDNA only max species length 
#trawl + eDNA max species length 
#trawl only individual length 

#merge original data w/ traits 
data2 <- merge(detection, trait_data, by="LCT", all.x= TRUE) #merge original data w/ traits 
data2 <- select(data2, c('LCT','gamma_detection_method', 'total_mean_length_cm'))#extract relevant columns 
colnames(data2) <- c('LCT', 'detection', 'length_cm') #change column names
catch <- select(trawl_catch, c('species', 'total_length_cm')) #select relevant columns
colnames(catch) <- c('LCT','length_cm') #change column names

#add column detection = trawl individuals
catch$detection <- c('trawl individuals')

#merge all data (trawl individuals and those from detection so we can see all distributions together)
length <- rbind(catch, data2) 

#plot
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


ggsave("./Outputs/traits/syn_length/all_length_dist_synop.png", 
       plot = plot,
       width = 10, height = 6, units = "in")


#Species Length Distributions w/ 'Both' Group
#plot density but differentiate by groups 
#we need to combine speciestraits to detection

method_2 <- select(detection, c('LCT','gamma_detection_method')) #select relevant column 

#rename species traits columns 
speciestraits <- speciestraits %>%  #rename column 
  rename(method = gamma_detection_method)


data <- merge(speciestraits, method_2, by="LCT", all.x= TRUE)
data <- distinct(data)

plot <- data %>%
  ggplot(aes(y = method)) +
  geom_density_ridges(
    aes(x = total_mean_length_cm , fill = gamma_detection_method), 
    alpha = 1, color = "white", from = 0, to = 150,
    bandwidth=13) +
  labs(
    x = "mean species length (cm)",
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


ggsave("./Outputs/traits/syn_length/trawl_edna_2_synop.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#Species Length Distributions (all on same line)
data$method <- c('detection')
data <- distinct(data)

plot <- data %>%
  ggplot(aes(y = method)) +
  geom_density_ridges(
    aes(x = total_mean_length_cm , fill = gamma_detection_method), 
    alpha = .8, color = "white", from = 0, to = 150,
    bandwidth=18) +
  labs(
    x = "mean species length (cm)",
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


ggsave("./Outputs/traits/syn_length/max_length_density.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

