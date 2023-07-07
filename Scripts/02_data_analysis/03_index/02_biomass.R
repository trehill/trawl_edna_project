#Biomass
#goal: compare biomass relationship between trawl + eDNA

#SET UP ####
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(geosphere)
library(hrbrthemes)
library(gmt)
library(scales)
library(skimr)

#function
#this function changes angle coordinates to decimal coordinates 
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

#read files 
beta_div <- read.csv(here::here("Processed_data", 
                                "datasets",
                                "detections_all.csv"),
                     head=TRUE)

trawl_meta <- read.csv(here::here("Processed_data", 
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

#only want to look at species PRESENT in trawl and BOTH, and spp with asssociated weight measurements 
beta_div <- subset(beta_div, gamma_detection_method %in% c('both eDNA/trawl', 'only trawl'))
beta_div <- subset(beta_div, weight_total_kg > 0 )
  #Ammodytes hexapterus, does not have any weight measurements from trawl (=0)
                        #many individuals 
  #Mallotus villosus, also no weight measurement, only 1 individual 

#Determien trawl distances ####
#determine distance of trawl by look at metadata + using function above 
trawl_meta <- trawl_meta %>% 
  rename(
    lat1 = start_latitude_n,
    lon1 = start_longitude_w,
    lat2 = end_latitude_n,
    lon2 = end_longitude_w
     )

#select specific points related to coordinates 
trawl <- select(trawl_meta, c('lat1', 'lon1','lat2', 'lon2', 'set_number'))

#change "." to "' 
trawl <- data.frame(lapply(trawl, function(x) {
  gsub(".", " ", x, fixed=TRUE) }))

#change day, min, sec to degrees
trawl$lat1 <- angle2dec(trawl$lat1)
trawl$lat2 <- angle2dec(trawl$lat2)
trawl$lon1 <- angle2dec(trawl$lon1)
trawl$lon2 <- angle2dec(trawl$lon2)

write_csv(trawl,
          here("Processed_data",
               "trawl",
               "metadata",
               'clean_data',
               "lat_lon_all.csv")) 

#Standardize 'biomass' by length  ####
#Do this by creating a biomass indices - we will call this biomass density 
#We will take biomass and divide by length of trawl 

#Determining 'length of trawl'
#use function w/ geodist to determine length of trawl (in km) from coordinates 
trawl_distances <- trawl %>% rowwise() %>% 
  mutate(distance = geodist(lat1, lon1, lat2, lon2, units=c("km")))

#calculate the mean, min + max distance of trawl 
mean(trawl_distances$distance) #16km 
min(trawl_distances$distance) #2.3 km
max(trawl_distances$distance) #27.2 km 

#merge to detection dataset 
data <- merge(trawl_distances, beta_div, by=c('set_number'), all.y=TRUE)

#create new column/variable that is biomass index (biomass(weight)/distance)
data$biomass_index <- data$weight_total_kg/data$distance

write_csv(data,
          here("Processed_data",
               "datasets",
               "biomass",
               "biomass_all.csv")) 

#Plot ####
#compare biomass density of BOTH to ONLY trawl using a box-plot
plot <- ggplot(data, aes(x=as.factor(gamma_detection_method), y=weight_total_kg)) + 
  geom_boxplot(fill= c("#00AFBB", "#5491cf"), alpha=1) + 
  scale_y_continuous(trans='log10', breaks=c(0, 0.01, 10, 100), labels=c('0', '0.01', '10','100')) +
  xlab("") + ylab("biomass density (kg/km)") +
  theme_classic()  


plot

#graph goes from 0.01 100, log 

ggsave("./Outputs/biomass/biomass_box_all.png", 
       plot = plot,
       width = 5, height = 5, units = "in")

#find mean of groups 
both <- subset(data, gamma_detection_method == c('both eDNA/trawl'))
trawl_o <- subset(data, gamma_detection_method == c('only trawl'))
mean(both$biomass_index) #1.109
mean(trawl_o$biomass_index) #0.04536

# Calculate standard errors for the 'both eDNA/trawl' group
se_both <- data %>%
  filter(gamma_detection_method == 'both eDNA/trawl') %>%
  summarise(se_biomass = sd(biomass_index) / sqrt(n()))

# Calculate standard errors for the 'only trawl' group
se_trawl_only <- data %>%
  filter(gamma_detection_method == 'only trawl') %>%
  summarise(se_biomass = sd(biomass_index) / sqrt(n()))

se_both$se_biomass  # Standard error for 'both eDNA/trawl' group, 0.338
se_trawl_only$se_biomass  # Standard error for 'only trawl' group, 0.038


#STAT ANALYSIS ####
#perform t-test 
#http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

#CHECK  T-TEST ASSUMPTIONS
#first we will log the data to standardize 

#check normality (p > 0.05)
# Shapiro-Wilk normality test for both biomass index
with(data, shapiro.test(biomass_index[gamma_detection_method == "both eDNA/trawl"])) #p = 1.128e-15

# Shapiro-Wilk normality test for only trawl biomass index 
with(data, shapiro.test(biomass_index[gamma_detection_method == "only trawl"])) #p=02.627e-0.6

#results: p values < 0.05 
#the data is not normally distributed
#we must standardize the data by logging it! 

data$biomass_index <- log(data$biomass_index)

#now check for normal distribution
with(data, shapiro.test(biomass_index[gamma_detection_method == "both eDNA/trawl"])) #p = 0.5786
with(data, shapiro.test(biomass_index[gamma_detection_method == "only trawl"])) #p=0.1936

#now we have a normal distribution

#check variance homogeneity 
res.ftest <- var.test(biomass_index ~ gamma_detection_method, data = data) #p=0.9401
res.ftest
#p > 0.05, there is difference in NO variance of two sets of data 

#T-test 
#P<0.05 = statistically significant 

#one-sided
t.test(biomass_index ~ gamma_detection_method, data=data)
#t=6.1864, df=20.661, p-value=4.4174e-06
#means are statistically difference 

#two-sided
#is both(ma) > only trawl(mb), Ha:mA>mB (greater), (one-tailed two-sample t test )
t.test(biomass_index ~ gamma_detection_method, data = data,
       var.equal = TRUE, alternative = "greater")

#t = 6.0423, df = 83, p-value= 1.80e-08

