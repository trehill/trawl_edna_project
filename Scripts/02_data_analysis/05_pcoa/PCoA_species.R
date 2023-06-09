#Principle Coordination Analysis 
#each point is a species 
#this plots spp by method on ordination plane using phyloseq


#SETUP####
#install.packages("ggside")
#install.packages("MicroViz")

#code to install MicroViz (not yet available on Cran)
#see: https://david-barnett.github.io/microViz/

#install.packages(
#  "microViz",
#  repos = c(davidbarnett = "https://david-barnett.r-universe.dev", getOption("repos"))
#)

library(tidyr)
library(dplyr)
library(phyloseq)
library(ggplot2)
library(ggside)
library(microViz)
library(RColorBrewer)


#read-data 
detection <- read.csv(here::here("Processed_data", 
                                 "datasets",
                                 "detections_all.csv"),
                      head=TRUE)

meta <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "metadata",
                            "clean_data",
                            "trawl_metadata2.csv"), #this is DIFFERENT than traw_metadata
                                                    #Site names have been changed to match other matrices created 
                 head=TRUE)

tax <- read.csv(here::here("Processed_data", 
                           "datasets",
                           "traits",
                           "traitdatabase.csv"),
                head=TRUE)

#Add regional column to traitdatabase ####
  #add regional column 
    #if spp is detected in North receives value N
    #if spp is detected in South receives value S
    #if spp is detected in both received N/S

traits <- tax #duplicate trait database 
              #subset trait database for species in detections 

detect_LCT <- detection$LCT
traits$detected = ifelse(traits$LCT %in% detect_LCT , tax$detected <- 
                      c("yes"), tax$detectedn<- c("no")) 

#subset for only detected spp.
traits <- subset(traits, detected == c('yes'))

  #(1) subset detection for those in the north and those in the south 
detection$set_number <- as.numeric(detection$set_number)

north <- subset(detection, set_number <= 7) #subset sets 1-7 as north

south <- subset(detection, set_number >= 7) #subset sets 8-16 as south


north_LCT <- north$LCT #list of all spp in north
north_LCT <- unique(north_LCT)
south_LCT <- south$LCT #list of all spp in south 
south_LCT <- unique(south_LCT)

bothnorth<- north[  north$LCT %in% south_LCT, ] #list of spp in both regions
bothsouth <- south[  south$LCT %in% north_LCT, ] #list of spp in both region

#unique(bothnorth$LCT) 
#unique(bothsouth$LCT) #yay they are the same! 

both = bothnorth

#we need to find LCT only found in north
bothnorth2 <- bothnorth$LCT #list of both spp. in north

onlynorth <- north[  !(north$LCT %in% bothnorth2), ] #list of spp only in trawl and NOT in both 
onlynorth <- na.omit(onlynorth) #should be less than original trawl df, remove NA

#we need to find LCT only found in south
bothsouth2 <- bothsouth$LCT #list of both spp. in eDNA

onlysouth <- south[  !(south$LCT %in% bothsouth2), ]  #list of spp only in eDNA and NOT in both 

#Add a new column called region: 
#Take each individual dataset and add a method that is either trawl,
#or eDNA and then I will merge them together

onlynorth = onlynorth$LCT
onlysouth = onlysouth$LCT
both = both$LCT

#add to trait database
north$region = ifelse(north$LCT %in% both , north$region <- 
                                       c("N/S"), north$region<- c("N")) 

#if the LCT is in the both list, then it receives the category N/S
#if not, the LCT is given the category N

south$region = ifelse(south$LCT %in% both , south$region <- c("N/S"), south$region <- c("S"))

#bind together 
region_df <- rbind(south, north)

#select relevant columns for traits 
region_df <- select(region_df, c('region', 'LCT'))
region_LCT <- distinct(region_df)

#merge to trait 

traits <- merge(traits, region_LCT, by=("LCT"), all.x=TRUE)

#if the LCT is in the both list, then it receives the category N/S
#if not, the LCT is given the category S


#(1)Create presence/absence matrix ####
#(rows = sites, columns= species) 

detection$presence <- 1 #add column of species presence (all species are present in this dataset)
spp_matrix <- select(detection, c('LCT', 'set_number', 'presence'))

spp_matrix <- spread(spp_matrix, LCT, presence)

spp_matrix <- spp_matrix %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) #replace all NA with 0 


matrix.please<-function(x) { #this function converts the first column of a df to the rownames of a matrix
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}

otumat <- matrix.please(spp_matrix) #make df a matrix 

rownames(otumat) <- paste0("Site", 1:nrow(otumat))

otumat

#(2) Create sample data (as dataframe) ####
#the row names must be the same as the row names of spp. matrix (in this case set_number)
#row = spp, column = detections, habitat

sample_table <- select(traits, c('LCT', 'habitat', 'region'))
sample_table <- sample_table %>%
  select(LCT, everything())

#add detection column 
method <- select(detection, c('LCT','gamma_detection_method'))
sample_mat <- merge(sample_table, method, by=c('LCT'), all.y=TRUE)
sample_mat <- distinct(sample_mat) #should be 40 spp observations 

#convert first column to row names in dataframe
sample_mat2 <- sample_mat[,-1]
rownames(sample_mat2) <- sample_mat[,1]

meta2 <- sample_mat2
meta2

#(3) Taxonomy data (as matrix) ####

tax <- meta %>%
  select(set_number, leg, everything()) %>%
  select(set_number, leg, area, date, depth_mean, duration_minutes) #set number is an integer 

  
tax_mat <- matrix.please(tax) #make df a matrix

tax_mat

#(4) Make phyloseq object ####
#make phyloseq object with these data 
OTU = otu_table(otumat, taxa_are_rows = TRUE) 
TAX = tax_table(tax_mat)

#make phyloseq object 
physeq = phyloseq(OTU, TAX) #wohooo it works!!!!
physeq = phyloseq(OTU, TAX, sample_data(meta2))

physeq #should have three different classes of object (otu_table, sample _data(), and tax_table())


#Plots ####
physeq %>% 
  dist_calc(dist = "jaccard", binary = TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "gamma_detection_method", shape= 'region', size = 2) +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) 


#Let's add boxplots on the side 
plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary=TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "gamma_detection_method",  size = 2, auto_caption = NA) +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf"),aesthetics = c("fill", "colour"), name = "detection" ) +
  theme_bw() +
  ggside::geom_xsideboxplot(aes(fill = gamma_detection_method, y = gamma_detection_method), orientation = "y") +
  ggside::geom_ysideboxplot(aes(fill = gamma_detection_method, x = gamma_detection_method), orientation = "x") +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 

plot 

#Let's add boxplots on the side, with area distinction
plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary=TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "gamma_detection_method", shape = 'region', size = 2, auto_caption = NA) +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf"),aesthetics = c("fill", "colour"), name = "detection" ) +
  theme_bw() +
  ggside::geom_xsideboxplot(aes(fill = gamma_detection_method, y = gamma_detection_method), orientation = "y") +
  ggside::geom_ysideboxplot(aes(fill = gamma_detection_method, x = gamma_detection_method), orientation = "x") +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 

plot 


#more plots (based on Max's code), using ggplot

PCOA <- ordinate(physeq, "PCoA", "jaccard", binary=TRUE)

plot <- plot_ordination(physeq, PCOA, type="samples", color="gamma_detection_method") +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) +
  theme_bw() +
  stat_ellipse() +
  ggtitle("BrayNMDS_temp_and_gulf") +
  labs(title = "PCoA ordination" , color = "Detection") 
  
plot

ggsave("./Outputs/pcoa/PCoA_basic.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


#add regional distinction

plot <- plot_ordination(physeq, PCOA, type="samples", color="gamma_detection_method", shape='region') +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) +
  theme_bw() +
  stat_ellipse() +
  labs(title = "PCoA ordination" , color = "Detection", shape= 'Region') 

plot


#Let's add boxplots on the side, but with the right colors 

plot <- plot_ordination(physeq, PCOA, type="samples", color="gamma_detection_method") +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) +
  theme_bw() +
  stat_ellipse() +
  labs(title = "PCoA ordination" , color = "Detection") +
  ggside::geom_xsideboxplot(aes(fill = NULL, y = gamma_detection_method), orientation = "y", outlier.shape=NA) +
  ggside::geom_ysideboxplot(aes(fill = NULL, x = gamma_detection_method), orientation = "x", outlier.shape=NA) +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 


plot

ggsave("./Outputs/pcoa/PCoA_bars.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#trying to add ellipses 

#Let's add boxplots on the side, with area distinction
plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary=TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "gamma_detection_method", shape = 'region', size = 2, auto_caption = NA) +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf"),aesthetics = c("fill", "colour"), name = "detection" ) +
  theme_bw() +
  stat_ellipse(aes(linetype = gamma_detection_method, colour = gamma_detection_method)) +
  ggside::geom_xsideboxplot(aes(fill = gamma_detection_method, y = gamma_detection_method), orientation = "y") +
  ggside::geom_ysideboxplot(aes(fill = gamma_detection_method, x = gamma_detection_method), orientation = "x") +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 

plot 

ggsave("./Outputs/pcoa/PCoA_final.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


  


