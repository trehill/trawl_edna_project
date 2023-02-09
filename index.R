#Exploring eDNA read index over biomass 

#SET UP ####
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(here)
library(dplyr)
library(ggplot2)
library(geosphere)

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

beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "beta_div.csv"),
                     head=TRUE)

beta_div <- subset(beta_div, p_abs_eDNA == 1)
beta_div <- subset(beta_div, weight_total_kg > 0 )

trawl_meta <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

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

trawl_distances <- trawl %>% rowwise() %>% 
  mutate(distance = distHaversine(c(lon1, lat1), c(lon2, lat2)))



#merge to beta div 

data <- merge(trawl_distances, beta_div, by=c('set_number'))
data$distance <- (data$distance)
data$set_read_index <- (data$set_read_index)

#create new column/variable that is biomass index (biomass(weight)/distance)
data$biomass_index <- data$weight_total_kg/data$distance

#plot 
plot <- ggplot(data,aes(set_read_index, biomass_index)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color='turquoise4') +
  theme_minimal() +
  labs(x='log(eDNA read index)', y='log(biomass index)',title='Biomass / Read Index') +
  theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  theme_classic()


plot 

ggsave("./Outputs/biomass/biomass_index.png", 
       plot = plot,
       width = 10, height = 6, units = "in")






  