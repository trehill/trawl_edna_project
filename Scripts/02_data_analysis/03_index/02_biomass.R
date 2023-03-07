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
library(scales)

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

write_csv(trawl,
          here("Processed_data",
               "trawl",
               "metadata",
               'clean_data',
               "lat_lon.csv")) 

#Standardize 'biomass' by length 
#Do this by creating a biomass 'indices' 
#We will take biomass and divide by length of trawl 

#Determining 'length of trawl'
#extract relevant columns 

#try using a different function w/ geodist
trawl_distances <- trawl %>% rowwise() %>% 
  mutate(distance = geodist(lat1, lon1, lat2, lon2, units=c("km")))

#calculate the mean, min + max distance of trawl 
mean(trawl_distances$distance) #16km 
min(trawl_distances$distance) #2.3 km
max(trawl_distances$distance) #27.2 km 

#merge to beta div 

data <- merge(trawl_distances, beta_div, by=c('set_number'))

#create new column/variable that is biomass index (biomass(weight)/distance)
data$biomass_index <- data$weight_total_kg/data$distance

write_csv(data,
          here("Processed_data",
               "traits",
               "biomass.csv")) 
#compare BOTH to ONLY trawl 

plot <- ggplot(data, aes(x=as.factor(gamma_detection_method), y=weight_total_kg)) + 
  geom_boxplot(fill="#00AFBB", alpha=0.2) + 
  scale_y_continuous(trans='log10', breaks=c(0, 0.01, 10, 100), labels=c('0', '0.01', '10','100')) +
  xlab("") + ylab("biomass index log(kg/km)") +
  theme_classic()  
plot

#find mean of groups 
both <- subset(data, gamma_detection_method == c('both eDNA/trawl'))
trawl_o <- subset(data, gamma_detection_method == c('only trawl'))
mean(both$biomass_index) #1.212
mean(trawl_o$biomass_index) #0.542



#graph goes from 0.01 100

ggsave("./Outputs/biomass/biomass_box.png", 
       plot = plot,
       width = 5, height = 5, units = "in")

#STAT ANALYSIS ####
#perform t-test 
#http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

#CHECK  T-TEST ASSUMPTIONS
#check normality (p > 0.05)
# Shapiro-Wilk normality test for both biomass index
with(data, shapiro.test(biomass_index[gamma_detection_method == "both eDNA/trawl"])) #p = 2.112e-14

# Shapiro-Wilk normality test for only trawl biomass index 
with(data, shapiro.test(biomass_index[gamma_detection_method == "only trawl"])) #p=5.609e-07

#results: p values < 0.05 
#the data is NOT normally distributed - must do wilcox t-test 

#check variance homogeneity 
res.ftest <- var.test(biomass_index ~ gamma_detection_method, data = data) #p< 2.2e-16
res.ftest
#p < 0.05, there is difference in variance of two sets of data 

#T-test 
#is there a difference in means (two-sided t test )
t.test(biomass_index ~ gamma_detection_method, data=data)
#t=3.004, df=64.153, p-value=0.003836
#mean in group both = 1.21249, mean = 0.05423
#means are statistically difference 

#is both(ma) > only trawl(mb), Ha:mA>mB (greater), (one-tailed two-sample t test )
t.test(biomass_index ~ gamma_detection_method, data = data,
       var.equal = TRUE, alternative = "greater")

#because the assumptions for the ttest are NOT met, we will perform a Wilcoxon test 

#Unpaired Two-Samples Wilcoxon Test
#http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r

#need to make data long 
dt <- select(data, c('gamma_detection_method', 'biomass_index'))
both_index <- subset(dt, gamma_detection_method == 'both eDNA/trawl')
colnames(both_index) <- c('method','both_index')
trawl_index <- subset(dt, gamma_detection_method == 'only trawl')
colnames(trawl_index) <- c('method','trawl_index')
data_wc<- cbind(both_index, trawl_index)

wilcox.test(data_wc$both_index, data_wc$trawl_index, alternative = "two.sided")
#We can conclude that both method median biomass index  is significantly different 
#from only trawl method median biomass index with a p-value = 1.642e-12.

#Now we can test whether both_eDNA/trawl biomass mean is statistically GREATER than the trawl biomass mean
wilcox.test(biomass_index ~ gamma_detection_method, data = data, 
            exact = FALSE, alternative = "greater")

#We can conclude that both method median biomass indexis significantly greater 
#from only trawl method median biomass index with a p-value = 4.285e-06

#mean both eDNA/trawl 


#mean only trawl 