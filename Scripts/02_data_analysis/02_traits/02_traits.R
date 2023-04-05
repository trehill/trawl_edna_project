#Trait Exploration New

#Set-Up ####
library(tidyr)
library(tidyverse)
library(RColorBrewer)
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

trawl_meta <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "metadata",
                            "clean_data",
                            "trawl_metadata.csv"),
                 head=TRUE)

eDNA_meta <- read.csv(here::here("Processed_data", 
                                  "eDNA",
                                  "metadata",
                                  "clean_data",
                                  "eDNA_metadata.csv"),
                       head=TRUE)

div_ind <- read.csv(here::here("Processed_data", 
                            "diversity",
                            "diversity_indices.csv"),
                 head=TRUE)



#create df, named data  ####

#merge identification data w/ trait data 
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)

#change 'both' to eDNA or trawl alone

#subset for 'both' data only 
both_to_eDNA <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to eDNA 
both_to_eDNA <- data.frame(lapply(both_to_eDNA, function(x) {
  gsub("both eDNA/trawl", "eDNA", x) 
  
}))

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
  gsub("only eDNA", "eDNA", x) 
  
}))

data <- data.frame(lapply(data, function(x) {
  gsub("only trawl", "trawl", x) 
  
}))


#let's plot!

#length ####

#check out ind. length info 
trawl_catch <- trawl_catch[!is.na(trawl_catch$total_length_cm),] #remove NA

#subset out 'fork length' we only want total length
#i don't know why we also have 'fork' length
#trawl_catch <- trawl_catch[trawl_catch$length_type == 'Total',]

plot <- ggplot(trawl_catch, aes(x=total_length_cm))+
  geom_histogram(color="#00AFBB", fill="#00AFBB", binwidth = 2,)+
  theme_classic()
plot

ggsave("./Outputs/traits/ind_length_his.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#let's just look at species 

speciestraits= subset(data, select = c(LCT, max_length_cm, gamma_detection_method) )
speciestraits <- distinct(speciestraits)
speciestraits <- speciestraits %>% drop_na(max_length_cm)



#species length distributions 
speciestraits$max_length_cm <- as.numeric(speciestraits$max_length_cm) 

plot <- ggplot(speciestraits, aes(max_length_cm)) +
  geom_density(aes(fill = gamma_detection_method), position = "stack") + 
  theme_classic()

plot

ggsave("./Outputs/traits/spp_length_dens.png", 
       plot = plot,
       width = 20, height = 12, units = "in")

plot <- ggplot(data, aes(max_length_cm)) +
  geom_density(aes(fill = gamma_detection_method), position = "stack") + 
  theme_classic()

plot

write_csv(speciestraits,
          here("Processed_data",
               "traits",
               "length_2.csv")) 


#FINAL LENGTH PLOTS #### 
#goal: make a density plot like above with four rows 
#trawl only max species length 
#eDNA only max species length 
#trawl + eDNA max species length 
#trawl only individual length 

#merge original data w/ traits 
data2 <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)
#extract relevant columns 
data2 <- select(data2, c('LCT','gamma_detection_method', 'max_length_cm'))
colnames(data2) <- c('LCT', 'detection', 'length_cm')

catch <- select(trawl_catch, c('species', 'total_length_cm')) 
#change column names
colnames(catch) <- c('LCT','length_cm')
#add column detection = trawl individuals

catch$detection <- c('trawl individuals')

#merge all data

length <- rbind(catch, data2) #this includes non-fish... should we get rid of them? 

write_csv(length,
          here("Processed_data",
               "traits",
               "length.csv")) 
#plot
plot <- ggplot(length, 
               aes(x = length_cm, 
                   y = detection, 
                   fill = detection)) +
  geom_density_ridges(bandwidth=5) + 
  theme_ridges() +
  labs("") +
  xlab("length (cm)") + ylab("") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#5491cf","#FCC442", "#00AFBB", "#9DE3E8")) +
  theme_classic()
plot 


ggsave("./Outputs/traits/length.png", 
       plot = plot,
       width = 10, height = 6, units = "in")


#another plot 
plot <- ggplot(speciestraits, 
               aes(x = max_length_cm, 
                   y = gamma_detection_method, 
                   fill = gamma_detection_method), show.legend = FALSE) +
  geom_density_ridges(bandwidth=20) + 
  theme_ridges() +
  xlab("maximum species length (cm)") +
  ylab("detection method") +
  scale_fill_manual(values=c("#FCC442", "#9DE3E8")) +
  theme(legend.position = "none") +
  theme_classic()
plot 


ggsave("./Outputs/traits/spp_length_dens2.png", 
       plot = plot,
       width = 10, height = 6, units = "in")



#plot density but differentiate by groups 



#we need to combine speciestraits to beta_div 

method_2 <- select(beta_div, c('LCT','gamma_detection_method'))

#rename species traits columns 
speciestraits <- speciestraits %>% 
  rename(method = gamma_detection_method)


data <- merge(speciestraits, method_2, by="LCT", all.x= TRUE)


plot <- data %>%
  ggplot(aes(y = method)) +
  geom_density_ridges(
    aes(x = max_length_cm , fill = gamma_detection_method), 
    alpha = .8, color = "white", from = 0, to = 250,
    bandwidth=18) +
  labs(
    x = "maximum species length (cm)",
    y = '') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c("#5491cf","#FCC442", "#00AFBB", "#9DE3E8"),
    name = "detection", guide = "legend"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE)

plot


####

ggsave("./Outputs/traits/length_bymethod.png", 
       plot = plot,
       width = 12, height = 7, units = "in")



#cut = trawl or eDNA 
#price = max length 
data2 <- data 
colnames(data2) <- c("LCT", "max_length_cm", "method","detection")

plot <- ggplot(data2, aes(max_length_cm)) +
  geom_density(aes(fill = detection), position = "stack", bw=12) + 
  scale_fill_manual(values=c("#5491cf","#FCC442", "#9DE3E8"))+
  scale_y_discrete(expand = c(0, 0)) +
  theme_classic() + 
  labs(
    x = "maximum species length (cm)") +
  facet_wrap(~method) 

plot

ggsave("./Outputs/traits/length_bymethod_stack.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


#all same row 
data$method <- c('detection')

plot <- data %>%
  ggplot(aes(y = method)) +
  geom_density_ridges(
    aes(x = max_length_cm , fill = gamma_detection_method), 
    alpha = .8, color = "white", from = 0, to = 250,
    bandwidth=18) +
  labs(
    x = "maximum species length (cm)",
    y = '') +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c("#5491cf","#FCC442", "#00AFBB", "#9DE3E8"),
    name = "detection", guide = "legend"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE)

plot


ggsave("./Outputs/traits/max_length_distribution.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


