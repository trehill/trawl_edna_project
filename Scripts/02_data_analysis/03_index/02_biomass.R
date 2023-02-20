#Biomass
#goal: compare biomass relationship between trawl + eDNA

#SET UP ####
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(here)
library(dplyr)
library(ggplot2)
library(geosphere)
library(hrbrthemes)
library(viridis)
library(gmt)

#function
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

beta_div <- read.csv(here::here("Processed_data", 
                                "datasets",
                                "detections.csv"),
                     head=TRUE)

trawl_meta <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

#only want to look at species PRESENT in trawl and BOTH 
beta_div <- subset(beta_div, gamma_detection_method %in% c('both eDNA/trawl', 'only trawl'))
beta_div <- subset(beta_div, weight_total_kg > 0 )

#determine distance of trawl by look at metadata + using function above 
trawl_meta <- trawl_meta %>% 
  rename(
    lat1 = start_latitude_n,
    lon1 = start_longitude_w,
    lat2 = end_latitude_n,
    lon2 = end_longitude_w
    
  )

#select specific points 
trawl <- select(trawl_meta, c('lat1', 'lon1','lat2', 'lon2', 'set_number'))

#change "." to "' 
trawl <- data.frame(lapply(trawl, function(x) {
  gsub(".", " ", x, fixed=TRUE) }))

#change day, min, sec to degrees
trawl$lat1 <- angle2dec(trawl$lat1)
trawl$lat2 <- angle2dec(trawl$lat2)
trawl$lon1 <- angle2dec(trawl$lon1)
trawl$lon2 <- angle2dec(trawl$lon2)


#Standardize 'biomass' by length 
#Do this by creating a biomass 'indices' 
#We will take biomass and divide by length of trawl 

#Determining 'length of trawl'
#extract relevant columns 

#try using a different function w/ geodist
trawl_distances <- trawl %>% rowwise() %>% 
  mutate(distance = geodist(lat1, lon1, lat2, lon2, units=c("km")))

#calculate the mean, min + max distance of trawl 
mean(trawl_distances$distance) 
min(trawl_distances$distance) 
max(trawl_distances$distance) 

#merge to beta div 

data <- merge(trawl_distances, beta_div, by=c('set_number'))

#create new column/variable that is biomass index (biomass(weight)/distance)
data$biomass_index <- data$weight_total_kg/data$distance

#compare BOTH to ONLY trawl 


data %>%
  ggplot( aes(x=gamma_detection_method, y=weight_total_kg, fill=gamma_detection_method)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")

plot <- ggplot(data, aes(x=as.factor(gamma_detection_method), y=weight_total_kg)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("detection") + ylab("biomass index (kg/km)") +
  theme_classic()
plot

ggsave("./Outputs/biomass/biomass_box.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#exclude the larger extremes
data <- subset(data, weight_total_kg < 20 )

plot <- ggplot(data, aes(x=as.factor(gamma_detection_method), y=weight_total_kg)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("detection") + ylab("biomass index (kg/km)") +
  theme_classic()
plot
