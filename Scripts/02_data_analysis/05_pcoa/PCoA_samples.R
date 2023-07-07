#Principle Coordinate Analysis 
#each point is a sample
#four categories: trawl, eDNA, north and south 

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
library(directlabels)


#read-data 
detection <- read.csv(here::here("Processed_data", 
                                 "datasets",
                                 "detections_all.csv"),
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


#Format detection data ####
#change' both' to trawl and eDNA at the SET level 

#change 'both' category to eDNA or trawl alone
#subset for 'both' data only 
data <- detection 

both_to_eDNA <- subset(data, alpha_detection_method == 'both eDNA/trawl')

#change all both to eDNA 
both_to_eDNA <- data.frame(lapply(both_to_eDNA, function(x) {gsub("both eDNA/trawl", "eDNA", x) }))

#same for trawl 
both_to_trawl <- subset(data, alpha_detection_method == 'both eDNA/trawl')

#change all both to trawl 
both_to_trawl <- data.frame(lapply(both_to_trawl, function(x) {
  gsub("both eDNA/trawl", "trawl", x) }))

#merge together 
both_fixed <- rbind(both_to_eDNA, both_to_trawl)

#merge back with our original data
without_both <- subset(data, alpha_detection_method != 'both eDNA/trawl')
data_new <- rbind(both_fixed, without_both)

#fix only... to simple trawl or eDNA 
data <- data.frame(lapply(data_new, function(x) {
  gsub("only eDNA", "eDNA", x) }))

data <- data.frame(lapply(data, function(x) {
  gsub("only trawl", "trawl", x) }))

#total of 16x2 sites (16 sites of eDNA, 16 sites of trawl)
#sites A = eDNA 
#sites B = trawl 

#first we will look at eDNA 
#in our detections dataset, the species detected by eDNA include those who have 'only eDNA' 
#and 'both' distinctions in the ALPHA level (by set)

#subset eDNA data

eDNA_spp <- subset(data, alpha_detection_method == 'eDNA')
eDNA_spp$set <- eDNA_spp$set_number #duplicate set number column (will be used later to plot)
eDNA_spp$set_number <- paste("A", eDNA_spp$set_number, sep = "") #add distinction to site 


#subset trawl data 

trawl_spp <- subset(data, alpha_detection_method == 'trawl')
trawl_spp$set <- trawl_spp$set_number #duplicate set number column (will be used later to plot)
trawl_spp$set_number <- paste("B", trawl_spp$set_number, sep = "") #add distinction to site 

#merge together 

matrix <- rbind(eDNA_spp, trawl_spp)

#Format meta data ####

#add 'B' to trawl set numbers 
trawl_meta$set <- trawl_meta$set_number #duplicate set column 
trawl_meta$set_number <- paste("B", trawl_meta$set_number, sep = "") #add distinction to site 
trawl_meta <- trawl_meta %>%
  select(set_number, date, area, depth_mean, leg, set) %>%   #put set_number column at the front
  rename(
    depth = depth_mean,
    region = leg
  )

trawl_meta$region_method <- paste("trawl", trawl_meta$region, sep = "_") #add column that includes region + method

#format eDNA metadata
eDNA_meta$set <- eDNA_meta$set_number #duplicate set column
eDNA_meta$set_number <- as.numeric(eDNA_meta$set_number)
eDNA_meta <- eDNA_meta[eDNA_meta$set_number >= 1 & eDNA_meta$set_number <= 16, ] #select sites 1-16
eDNA_meta <- eDNA_meta[eDNA_meta$depth != 5,]
eDNA_meta$set_number <- paste("A", eDNA_meta$set_number, sep = "") #add 'A' to meta set numbers

eDNA_meta <- eDNA_meta %>%
  select(set_number, date, area, depth, north_south, set) %>%  
  rename(region = north_south) %>% #put set_number column at the front
  distinct() 

eDNA_meta <- eDNA_meta[complete.cases(eDNA_meta), ]

eDNA_meta <- data.frame(lapply(eDNA_meta, function(x) {
  gsub('S', "southern", x) }))
eDNA_meta <- data.frame(lapply(eDNA_meta, function(x) {
  gsub('N', "northern", x) }))

eDNA_meta$region_method <- paste("eDNA", eDNA_meta$region, sep = "_") #add column that includes region + method

#add column variable for whether its eDNA or trawl 
eDNA_meta$method <- c('eDNA')
trawl_meta$method <- c('trawl')
#bind 

meta <- rbind(eDNA_meta, trawl_meta)

#1) make species absence/presence matrix ####
#total of 16x2 sites (16 sites of eDNA, 16 sites of trawl)
#sites A = eDNA 
#sites B = trawl 

#make matrix that has species as rows, and sites/sets as columns 

matrix$presence <- 1 #add column of species presence (all species are present in this dataset)
spp_matrix <- select(matrix, c('LCT', 'set_number', 'presence'))

spp_matrix <- spread(spp_matrix, set_number, presence) #convert data from long to wide (so that sites are columns)

spp_matrix <- spp_matrix %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) #replace all NA with 0 


matrix.please<-function(x) { #this function converts the first column of a df to the rownames of a matrix
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}

otumat <- matrix.please(spp_matrix) #make df a matrix 
otumat

#2) create sample table 

#convert first column to row names in dataframe
sample_mat2 <- meta[,-1]
rownames(sample_mat2) <- meta[,1]

meta2 <- sample_mat2
meta2


#(3) Make phyloseq object ####
#make phyloseq object with these data 
OTU = otu_table(otumat, taxa_are_rows = TRUE) 
physeq = phyloseq(OTU, sample_data(meta2))

#make phyloseq object 
physeq 

#do some plotting

#PLOTS ####
physeq %>% 
  dist_calc(dist = "jaccard", binary = TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "method", shape= 'region', size = 2) +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) 

#Let's add boxplots on the side 
plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary=TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "method",  size = 2, auto_caption = NA) +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf"),aesthetics = c("fill", "colour"), name = "detection" ) +
  theme_bw() +
  ggside::geom_xsideboxplot(aes(fill = method, y = method), orientation = "y") +
  ggside::geom_ysideboxplot(aes(fill = method, x = method), orientation = "x") +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 

plot 

#Let's add boxplots on the side, with area distinction
plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary=TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "method", shape = 'region', size = 2, auto_caption = NA) +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf"),aesthetics = c("fill", "colour"), name = "detection" ) +
  theme_bw() +
  ggside::geom_xsideboxplot(aes(fill = method, y = method), orientation = "y") +
  ggside::geom_ysideboxplot(aes(fill = method, x = method), orientation = "x") +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 

plot 


#Plots using ggplot

PCOA <- ordinate(physeq, "PCoA", "jaccard", binary=TRUE)

plot <- plot_ordination(physeq, PCOA, type="samples", color="method") +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) +
  theme_bw() +
  stat_ellipse() +
  ggtitle("BrayNMDS_temp_and_gulf") +
  labs(title = "PCoA ordination" , color = "Detection") 

plot

ggsave("./Outputs/pcoa/PCoA1.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


#add regional distinction

plot <- plot_ordination(physeq, PCOA, type="samples", color="method", shape='region') +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) +
  theme_bw() +
  stat_ellipse() +
  labs(title = "PCoA ordination" , color = "Detection", shape= 'Region') 

plot


#Let's add boxplots on the side, but with the right colors 

plot <- plot_ordination(physeq, PCOA, type="samples", color="method") +
  scale_color_manual(values = c("#00AFBB","#FCC442", "#5491cf")) +
  theme_bw() +
  stat_ellipse() +
  labs(title = "PCoA ordination" , color = "Detection") +
  ggside::geom_xsideboxplot(aes(fill = NULL, y = method), orientation = "y", outlier.shape=NA) +
  ggside::geom_ysideboxplot(aes(fill = NULL, x = method), orientation = "x", outlier.shape=NA) +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 


plot

ggsave("./Outputs/pcoa/PCoA2.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#trying to add ellipses 

#Let's add boxplots on the side, with area distinction

plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary=TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "method", shape = 'method', size = 2) +
  scale_color_manual(values = c("#FCC442","#5491cf"),aesthetics = c("fill", "colour"), name = "legend" ) +
  theme_bw() +
  stat_ellipse(aes(linetype = region)) +
  ggside::geom_xsideboxplot(aes(fill = method, y = method), orientation = "y") +
  ggside::geom_ysideboxplot(aes(fill = method, x = method), orientation = "x") +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 

plot 

#add elipses

plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary=TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "method", size = 2) +
  scale_color_manual(values = c("#FCC442","#5491cf"),aesthetics = c("fill", "colour"), name = "method" ) +
  theme_bw() +
  stat_ellipse(aes(linetype = region)) +
  ggside::geom_xsideboxplot(aes(fill = method, y = method), orientation = "y") +
  ggside::geom_ysideboxplot(aes(fill = method, x = method), orientation = "x") +
  ggside::scale_xsidey_discrete(labels = NULL) +
  ggside::scale_ysidex_discrete(labels = NULL) +
  ggside::theme_ggside_void() 

plot


#let's take the elipses out- every point is either N/S and either eDNA or trawl 
#color = detection
#shape = region

plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary = TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "method", shape= 'region', size = 2) +
  scale_color_manual(values = c("#FCC442", "#5491cf")) +
  stat_ellipse(aes(linetype = region)) 

plot

ggsave("./Outputs/pcoa/PCoA_spatial.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#make ordination where each sample is connected by a line to its corresponding set
#ie. eDNA sample from set 1 is connected to trawl sample from set 1

# Calculate distance and perform ordination
distances <- dist_calc(physeq, dist = "jaccard", binary = TRUE)
ordination <- ord_calc(distances, "PCoA")

# Create the ordination plot
plot <- ord_plot(ordination, color = "method", shape = "region", size = 2, clip = 'on') +
  scale_color_manual(values = c("#FCC442", "#5491cf")) +
  geom_line(aes(group = set, linetype = region), color = "black") +  # Set linetype based on region
  geom_dl(aes(label = set), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = set), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8))

plot


ggsave("./Outputs/pcoa/PCoA_panel1.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#make plot with four ovals 
  #1. trawl S
  #2. trawl N
  #3. eDNA S
  #4. eDNA N 

plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary = TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "method", shape= 'region', size = 2) +
  scale_color_manual(values = c("#FCC442", "#5491cf")) +
  stat_ellipse(aes(linetype = region_method, color=method)) +
  scale_linetype_manual(values = c(trawl_northern = "solid", trawl_southern = "dashed", eDNA_southern = 'dashed', eDNA_northern = 'solid'))


plot

ggsave("./Outputs/pcoa/PCoA_panel2.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

#plot 
#color = north or south region 
#points = filled trawl, open eDNA
#color of lines matches north or south region
# Calculate distance and perform ordination
distances <- dist_calc(physeq, dist = "jaccard", binary = TRUE)
ordination <- ord_calc(distances, "PCoA")

# Define custom shapes
# Replace "1" and "2" with the desired shape values
custom_shapes <- c("eDNA" = 1, "trawl" = 16)

# Create the ordination plot
plot <- ord_plot(ordination, color = "region", shape = "method", size = 4, clip = 'on') +
  scale_color_manual(values = c("#FCC442", "#5491cf")) +
  scale_shape_manual(values = custom_shapes) +  # Set custom shapes
  geom_line(aes(group = set, color = region)) +  # Set linetype and color based on region
  geom_dl(aes(label = set), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  geom_dl(aes(label = set), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8)) +
  coord_cartesian(xlim = c(-2, 2))

plot

ggsave("./Outputs/pcoa/PCoA_panel1.png", 
       plot = plot,
       width = 12, height = 7, units = "in")


#make plot with four ovals 
#1. trawl S
#2. trawl N
#3. eDNA S
#4. eDNA N 

#we want northern to be yellow, and all southern to be blue 
#we want eDNA to be dashed and unfilled
#we want trawl to be solid and filled 
plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary = TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "region", shape = "region_method", size = 2) +
  scale_color_manual(values = c("#FCC442", "#5491cf")) +
  scale_shape_manual(values = c(eDNA_northern = 1, eDNA_southern = 1, trawl_northern = 16, trawl_southern = 16)) +
  stat_ellipse(aes(linetype = region_method, color = region)) +
  scale_linetype_manual(values = c(trawl_northern = "solid", trawl_southern = "solid", eDNA_southern = 'dashed', eDNA_northern = 'dashed'))

plot


#PLOTS w/ convex hulls instead of ellipses 


plot <- physeq %>% 
  dist_calc(dist = "jaccard", binary = TRUE) %>% 
  ord_calc("PCoA") %>% 
  ord_plot(color = "region", shape = "region_method", size = 2) +
  scale_color_manual(values = c("#FCC442", "#5491cf")) +
  scale_shape_manual(values = c(eDNA_northern = 1, eDNA_southern = 1, trawl_northern = 16, trawl_southern = 16)) +
  stat_chull(aes(linetype = region_method, color = region)) +
  scale_linetype_manual(values = c(trawl_northern = "solid", trawl_southern = "solid", eDNA_southern = 'dashed', eDNA_northern = 'dashed'))

plot

ggsave("./Outputs/pcoa/PCoA_panel2.png", 
       plot = plot,
       width = 12, height = 7, units = "in")

