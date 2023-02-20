#DEPTH 
#Exploring depth difference of sampling as a driver for difference in community structure

#Set-Up ####
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)


#set-up, read in files ####

beta_div <- read.csv(here::here("Processed_data", 
                                "datasets",
                                "detections.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)

trawl_catch  <- read.csv(here::here("Processed_data", 
                                    "trawl",
                                    "catch_data",
                                    "clean_data",
                                    "trawl_catch_clean.csv"),
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
                               "diversity_indices_all.csv"),
                    head=TRUE)

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
  labs(x='Difference in Depth Between Sample Methods', y='Jaccards Index (beta diversity)',title='Beta Diversity / Depth') +
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

write_csv(shared_depth,
          here("Processed_data",
               "traits",
               "depth.csv")) 

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

#trying to plot all of this on the same graph 
#reformat df, turnover + nestedness will become categorical variables 
new_df <- select(shared_depth, c('Jac_turn','Jac_nest', 'depth_difference'))
colnames(new_df) <- c('turnover','nestedness','depth_difference')

data_long <- gather(new_df, index, measurement, turnover:nestedness, factor_key=TRUE)

ggplt <- ggplot(data_long,aes(x=depth_difference,y=measurement,shape=index, color=index))+
  labs(x='Difference in Depth Between Sampling Methods', y='Jaccards Indices') +
  geom_point()+
  theme_classic()

ggplt

# Plotting multiple Regression Lines
plot <- ggplt+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
                  aes(color=index))
plot

ggsave("./Outputs/depth/nest_turn.png", 
       plot = plot,
       width = 10, height = 6, units = "in")
