#create PCoA 

#Set-Up ####
#Install packages 
#download phyloseq package 
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#BiocManager::install("phyloseq")

#load libraries 
library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(phyloseq)
library(ape)

#read data 
#ASVmatrix <- read.csv(here::here("Processed_data",  #ASV matrix
#                            "eDNA",
#                            "12s",
#                            "12se",
#                            "asv",
#                            "matrix",
#                            "clean_data",
#                            "data12se_asvmatrix_nc_lor.csv"),
#                 head=TRUE)


sample_data <- read.csv(here::here("Processed_data",  #sample data
                           "eDNA",
                           "metadata",
                           "clean_data",
                           "eDNA_metadata2.csv"),
                head=TRUE)
#format data 
ASV<- ASVmatrix%>% 
  rename(sample_name = X)  #rename first column 

Symportal <- arrange(ASV, sample_name)
Symportal <- na.omit(Symportal)  #remove NA
row.names(Symportal) <- NULL

sample_data <- arrange(sample_data, sample_name)
sample_data <- na.omit(sample_data)#remove NA 
row.names(sample_data) <- NULL

# Make a phyloseq object
otu_table1 <-  data.matrix(Symportal, rownames.force = NA) #make numeric matrix from df 
sample_data1 <- sample_data


#This one has NAs filtered out

otu <- otu_table(otu_table1, taxa_are_rows = FALSE)
sam <- sample_data(sample_data1)
ps1 <- phyloseq(otu, sam)
taxa_names(ps1) <- paste0("asv", seq(ntaxa(ps))) #this tricks phyloseq into using ASV instead of OTU
ps1

#plot ordination
set.seed(1789)
MDSbray <- ordinate(ps1, "NMDS", "bray")

#plot 
#plot the ordination (nMDS)
NMDSplot_temp <- plot_ordination(ps1, MDSbray, type="samples", color="north_south") +
  scale_color_manual(values = c("dodgerblue2", "gold")) +
  theme_bw() +
  stat_ellipse() + 
  ggtitle("BrayNMDS_temp") +
  labs(title = "NMDS ordination by region", color = "north_south")
NMDSplot_temp

#try to do this w/ trawl data now 

trawl <- read.csv(here::here("Processed_data",  #sample data
                                   "trawl",
                                   "catch_data",
                                   "clean_data",
                                   "trawl_catch_sum.csv"),
                        head=TRUE)


sample_data <- read.csv(here::here("Processed_data",  #sample data
                                   "trawl",
                                   "metadata",
                                   "clean_data",
                                   "trawl_metadata.csv"),
                        head=TRUE)

