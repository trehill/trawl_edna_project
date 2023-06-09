#Species Accumulation Curve with Rarefaction 
#create spp accumulation curves 

#1. install/load libraries
#2. read data
#3. create species presence absence matrix 
#4. convert to iNEXT input using as.infreq()
#5. plot spp. accumulation curve for ALL (trawl+eDNA) incidence 
#6. parse data into eDNA incidence and trawl incidence 
#7. make individual spp. matrix for eDNA and for trawl for each set
#8. plot individual spp. accumulation curve for trawl and eDNA
#9. attempt plotting eDNA and trawl on same curve 
  #data would look like this: 
  #trawl [16]: summed number of incidence per spp across each site 
  #eDNA [16]: summer number of incidence per spp across each site 
#10. plot spp accumulation comparing trawl + eDNA 

#(1) install/load libraries ####
#install.packages('iNEXT') 
    #interpolation and extrapolation for species diversity
    #information about this package: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web//packages/iNEXT/iNEXT.pdf

library(iNEXT)
library(tidyr)
library(dplyr)
library(ggplot2)

#(2) read data ####
#read-data 
detection <- read.csv(here::here("Processed_data", 
                                 "datasets",
                                 "detections_all.csv"),
                      head=TRUE)

#include function (for making matrices)
matrix.please<-function(x) { #this function converts the first column of a df to the rownames of a matrix
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}

#(3) create spp. presence/absence matrix ####

#make spp matrix df - where taxa are rows and sites are columns
detection$presence <- 1 #add column of species presence (all species are present in this dataset)

spp_matrix <- select(detection, c('LCT', 'set_number', 'presence'))
spp_matrix <- spread(spp_matrix, set_number, presence) #spread data wide 
spp_matrix <- spp_matrix %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) #replace all NA with 0 

#spp_matrix is our df that we will use to compute the spp accumulation curve
df <- matrix.please(spp_matrix) #make df a matrix 

df #view 

#(4) convert to iNEXT input using as.infreq() ####
#transform incidence raw data to incidence frequencies 

incfreq <- as.incfreq(df)
incfreq

#create iNEXT object to be able to plot 
out <-iNEXT(incfreq, q=0,datatype="incidence_freq",se=TRUE) #make iNext object 

#(5) plot spp. accumulation curve for ALL (trawl+eDNA) incidence ####

plot <- ggiNEXT(out, type=1, color.var="Order.q")+ 
  scale_color_manual(values=c("#b9d2eb"))+
  labs(title="",
        x ="Number of Sites", y = "Species Incidence") +
  scale_fill_manual(values=c("#5491cf"))  +
  theme_classic() + 
  theme(legend.position="None")
                             
plot 

ggsave("./Outputs/accumcurves/curve_all.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#(6) parse data into eDNA incidence and trawl incidence ####

#change eDNA/trawl to both 
#change 'both' category to eDNA or trawl alone
#subset for 'both' data only 
data <- detection 

both_to_eDNA <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to eDNA 
both_to_eDNA <- data.frame(lapply(both_to_eDNA, function(x) {gsub("both eDNA/trawl", "eDNA", x) }))

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

#subset trawl + eDNA 

trawl <- subset(data, gamma_detection_method == c('trawl'))
trawl$set_number <- as.numeric(trawl$set_number) #make set number numeric 
eDNA <- subset(data, gamma_detection_method == c('eDNA'))
eDNA$set_number <- as.numeric(eDNA$set_number) #make set number numeric 

#(7) make spp matrix df for eDNA and trawl separately ####
#make spp matrix for trawl 
trawl$presence <- 1 #add column of species presence (all species are present in this dataset)

matrix_trawl <- select(trawl, c('LCT', 'set_number', 'presence'))
matrix_trawl <- spread(matrix_trawl, set_number, presence) #spread data wide 
matrix_trawl <- matrix_trawl %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) #replace all NA with 0 

df_trawl <- matrix.please(matrix_trawl) #make df a matrix 
df_trawl

#make spp matrix for eDNA 
eDNA$presence <- 1 #add column of species presence (all species are present in this dataset)

matrix_eDNA <- select(eDNA, c('LCT', 'set_number', 'presence'))
matrix_eDNA <- spread(matrix_eDNA, set_number, presence) #spread data wide 
matrix_eDNA <- matrix_eDNA %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) #replace all NA with 0 

df_eDNA <- matrix.please(matrix_eDNA) #make df a matrix 
df_eDNA

#(8) make iNEXT objects + plot for trawl and eDNA indv. ####

#TRAWL 

incfreq_trawl <- as.incfreq(df_trawl)
incfreq_trawl

#create iNEXT object to be able to plot 
out <-iNEXT(incfreq_trawl, q=0,datatype="incidence_freq",se=TRUE)

plot <- ggiNEXT(out, type=1, color.var="Order.q")+ 
  scale_color_manual(values=c("#b9d2eb"))+
  labs(title="",
       x ="Number of Sites", y = "Trawl Species Incidence") +
  scale_fill_manual(values=c("#5491cf"))  +
  theme_classic() + 
  theme(legend.position="None")

plot 

ggsave("./Outputs/accumcurves/curve_trawl.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#eDNA 

incfreq_eDNA <- as.incfreq(df_eDNA)
incfreq_eDNA

#create iNEXT object to be able to plot 
out <-iNEXT(incfreq_eDNA, q=0,datatype="incidence_freq",se=TRUE)

plot <- ggiNEXT(out, type=1, color.var="Order.q")+ 
  scale_color_manual(values=c("#b9d2eb"))+
  labs(title="",
       x ="Number of Sites", y = "eDNA Species Incidence") +
  scale_fill_manual(values=c("#5491cf"))  +
  theme_classic() + 
  theme(legend.position="None")

plot 

ggsave("./Outputs/accumcurves/curve_eDNA.png", 
       plot = plot,
       width = 12, height = 7, units = "in")
                    
#(9) attempt plotting eDNA and trawl on same curve ####
#data would look like this: 
#trawl [16]: summed number of incidence per spp across each site 
#eDNA [16]: summer number of incidence per spp across each site 

both <- list(incfreq, incfreq_trawl, incfreq_eDNA)
names(both) <- c("All", "Trawl", "eDNA")  

out <-iNEXT(both, q=0,datatype="incidence_freq",se=TRUE)

#(10) plot spp accumulation comparing trawl + eDNA 
plot <- ggiNEXT(out, type=1, color.var="Assemblage")+ 
  scale_color_manual(values=c("#00AFBB","#FCC442","#5491cf"))+
  labs(title="",
       x ="Number of Sites", y = "Species Incidence") +
  scale_fill_manual(values=c("#00AFBB","#FCC442","#5491cf"))  +
  theme_classic() 

plot 

ggsave("./Outputs/accumcurves/curve_all_final.png", 
       plot = plot,
       width = 12, height = 7, units = "in")
