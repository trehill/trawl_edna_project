#visualizing metadata (ie location of eDNA + differences in sampling depth)

#set-up
library(tidyr)
library(ggplot2)
library(here)
library(dplyr)

#files = eDNA metadata + trawl metadata

metatrawl <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "metadata", 
                            "clean_data",
                            "trawl_metadata.csv"),
                 head=TRUE)

metatrawl<- select(metatrawl, c('set_number','depth_mean'))

metaeDNA <- read.csv(here::here("Processed_data", 
                                 "eDNA",
                                 "metadata", 
                                 "clean_data",
                                 "eDNA_metadata.csv"),
                      head=TRUE)

metaeDNA <- select(metaeDNA, c('set_number', 'depth'))
                  
#change set_number to integer
metaeDNA$set_number <- as.numeric(metaeDNA$set_number)
metaeDNA <- metaeDNA %>% drop_na(set_number)
metaeDNA <- metaeDNA[!is.na(metaeDNA$depth),]

#merge files 

meta <- merge(metatrawl, metaeDNA, by=c('set_number'))

meta_new <- meta %>%
  group_by(set_number) %>%
  dplyr::summarise(eDNA_mean_depth = mean(depth))

meta_all <- merge(meta, meta_new, by=c('set_number'))
meta_all <- select(meta_all, c('set_number','depth_mean','eDNA_mean_depth'))
meta_all <- distinct(meta_all)

#explore depths of trawl and eDNA samples
plot <- meta_all %>%
  ggplot(aes(x=as.factor(set_number), y=eDNA_mean_depth, col="eDNA")) + 
  geom_jitter()+
  geom_point(aes(y=depth_mean, col="trawl"), size=1.5) + 
  scale_color_manual(values = c("#00AFBB", "#132B43")) +
  scale_y_reverse() + theme_bw() +
  labs(y="depth (m)", x="site")  + 
  theme(legend.title= element_blank())

plot


plot <- meta_all %>%
  ggplot(aes(x=as.factor(set_number), y=eDNA_mean_depth, col="eDNA")) + 
  geom_jitter()+
  geom_point(aes(y=depth_mean, col="trawl"), size=1.5) + 
  scale_color_manual(values = c('steelblue3', "#132B43")) +
  scale_y_reverse() + theme_bw() +
  labs(y="depth (m)", x="site")  + 
  theme(legend.title= element_blank())

plot

ggsave("./Outputs/metadata/samplingdepths.png", 
       plot = plot,
       width = 10, height = 5, units = "in")


#rename columns 
colnames(meta_all) <- c('set','trawl','eDNA')
#make data long

data_long <- gather(meta_all, type, depth, trawl:eDNA, factor_key=TRUE)


plot <- data_long %>%
  ggplot(aes(x=as.factor(set), y=depth, col=type)) + 
  geom_jitter()+
  geom_point(aes(y=depth, col=type), size=1.5) + 
  scale_color_manual(values = c("#00AFBB", "#132B43")) +
  scale_y_reverse() + theme_bw() +
  labs(y="depth", x="site")  + 
  theme(legend.title= element_blank())

plot


ggsave("./Outputs/metadata/samplingdepths2.png", 
       plot = plot,
       width = 10, height = 5, units = "in")

