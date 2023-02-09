#Trait Exploration New

#Set-Up ####
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)

#set-up, read in files ####

beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "beta_div.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)

trawl_catch  <- read.csv(here::here("Processed_data", 
                                    "trawl",
                                    "catch_data",
                                    "clean_data",
                                    "trawl_catch.csv"),
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
                            "datasets",
                            "diversity_indices.csv"),
                 head=TRUE)

#notes: 
#need to look at LCT for trawl species #Sebastes sp. + #Corphaenidoes sp
#need to define 'environment' 

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
trawl_catch <- trawl_catch[!is.na(trawl_catch$length_cm),] #remove NA

plot <- ggplot(trawl_catch, aes(x=length_cm))+
  geom_histogram(color="lightblue", fill="lightblue", binwidth = 4,)+
  theme_classic()
plot

ggsave("./Outputs/traits/ind_length_his.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#let's just look at species 

speciestraits= subset(data, select = c(LCT, max_length_cm, gamma_detection_method) )
speciestraits <- distinct(speciestraits)

#species length distributions 
speciestraits$max_length_cm <- as.numeric(speciestraits$max_length_cm) 

plot <- ggplot(speciestraits, aes(max_length_cm)) +
  geom_density(aes(fill = gamma_detection_method), position = "stack") + 
  theme_classic()

plot

ggsave("./Outputs/traits/spp_length_dens.png", 
       plot = plot,
       width = 20, height = 12, units = "in")

#another plot 
plot <- ggplot(speciestraits, 
       aes(x = max_length_cm, 
           y = gamma_detection_method, 
           fill = gamma_detection_method)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs("") +
  theme(legend.position = "none") +
  theme_classic()
plot 

ggsave("./Outputs/traits/spp_length_dens2.png", 
       plot = plot,
       width = 10, height = 6, units = "in")


#habitat/depth ####

#check out environment 
plot <- ggplot(data = data, aes(x = environment, fill = gamma_detection_method)) +
  geom_bar() + 
  theme_classic()
plot 

ggsave("./Outputs/traits/habitat_hist.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#create mean_depth trait per species (average of min / max)
#add new column that is the average of the depth range in fishbase per species 
data$depth_range_min <- as.numeric(data$depth_range_min) 
data <- data %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

data$depth_range_max <- as.numeric(data$depth_range_max) 
data <- data %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

data <- mutate(data %>% rowwise(),
               avr_depth_sp = rowMeans(cbind(depth_range_min,depth_range_max)))

#need to add metadata  - maybe this is something to discuss w/ focus group  
#extract relevant columns from meta 
meta_min <- subset(meta, select = c(depth_mean, net_tow) )
meta_min <- meta_min %>% rename(set_number = net_tow)

data$set_number <- as.numeric(data$set_number) 
data_meta <- merge(data, meta_min, by=c('set_number'))


#difference between spp average depth vs. trawl average depth + color in detection method 

#create new column that is the difference between avr_depth_sp + trawl mean depth 
data_meta$diff_in_depth <- abs(data_meta$avr_depth_sp - data_meta$depth_mean)

#point plot 
plot <- ggplot(data_meta, aes(diff_in_depth)) +
geom_density(aes(fill = gamma_detection_method), position = "stack") + 
  theme_classic()
plot 

ggplot(data_meta, 
       aes(x = avr_depth_sp, 
           y = depth_mean)) +
  geom_point()

ggplot(data_meta, aes(x=avr_depth_sp, y=depth_mean, color=gamma_detection_method)) +
  geom_point(alpha=0.5) +
  theme_classic()

#make count/occurence 
df <- data_meta %>%
  #group by sample and taxonomy
  group_by(LCT, gamma_detection_method) %>%
  tally()

new_df <- merge(df, data_meta, by=c('LCT', 'gamma_detection_method' ))
new_df <- distinct(new_df)


new_df %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=avr_depth_sp, y=depth_mean, size=n, color=gamma_detection_method)) +
  geom_point(alpha=0.25) +
  scale_size(range = c(2, 10), name="occurence of detection") +
  theme_classic()


#check out https://r-graph-gallery.com/320-the-basis-of-bubble-plot.html

#box plot
data_meta$diff_in_depth <- as.character(data_meta$diff_in_depth)

#DEPTH 

#determine depth difference between trawl + eDNA sampling 


#DEPTH 

#need to make difference between trawl + eDNA sampling depth + Jaccards indices 

eDNA_meta <- select(eDNA_meta, c('set_number', 'depth'))
eDNA_meta <- subset(eDNA_meta, depth != 5) #exclude 5m 
eDNA_meta <- eDNA_meta %>% drop_na(depth)
eDNA_meta <- eDNA_meta %>% rename(depth_eDNA = depth,)
eDNA_meta <- distinct(eDNA_meta)

trawl_meta <- select(trawl_meta, c('set_number', 'depth_mean'))
trawl_meta <- trawl_meta %>% rename(depth_trawl = depth_mean,)
trawl_meta <- distinct(trawl_meta)

depth_set <- merge(trawl_meta, eDNA_meta, by=c('set_number'))

#linear regression beta div (jaccards index) over depth difference ####
depth_beta <- merge(depth_set, div_ind, by=c('set_number'))
depth_beta <- depth_beta %>% drop_na(depth_eDNA)
depth_beta <- distinct(depth_beta)

#add new column for difference in depth 
depth_beta$depth_difference <- abs(depth_beta$depth_eDNA - depth_beta$depth_trawl)

#plot 
#jaccards index over depth difference 

plot <- ggplot(depth_beta,aes(depth_difference, Jac)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Jaccards Index',title='Beta Diversity / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()
plot 

ggsave("./Outputs/depth/depth_jac.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#nestedness over depth difference 

plot <- ggplot(depth_beta,aes(depth_difference, Jac_nest)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Jaccards Nestedness Index',title='Nestedness / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()
plot 

ggsave("./Outputs/depth/depth_nest.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#turnover over depth difference 
plot <- ggplot(depth_beta,aes(depth_difference, Jac_turn)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Jaccards Turnover Index',title='Turnover / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()
plot 

ggsave("./Outputs/depth/depth_turn.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#shared species over depth difference 
shared <- merge(beta_div, depth_beta, by=c('set_number'))
x <- shared

df2 <- x %>% group_by(set_number, alpha_detection_method) %>% 
  summarise(shared_count=n(),.groups = 'drop')

df3 <- df2 %>% group_by(set_number) %>% 
  summarise(total_count = sum(shared_count))

df4 <- merge(df2, df3, by=c('set_number'))

#subset for method = both 
df5 <- subset(df4, alpha_detection_method == c('both eDNA/trawl'))
df5$shared_over_total <- abs(df5$shared_count/df5$total_count)

#combine to depth difference 

shared_depth <- merge(depth_beta, df5, by=c('set_number'))

plot <- ggplot(shared_depth,aes(depth_difference, shared_over_total)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='Difference in Depth Between Sample Methods', y='Number Shared Species / Total Number Species',title='Shared Species / Depth') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()

plot
ggsave("./Outputs/depth/depth_shared.png", 
       plot = plot,
       width = 10, height = 6, units = "in")
