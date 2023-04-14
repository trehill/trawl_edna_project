#Converting Fork to Total Length for Species Caught in Trawl 

#By searching literature OR 
#FISHBASE using length-length table
#https://www.fishbase.se/manual/english/PDF/FB_Book_CBinohlan_Length-Length_RF_JG.pdf

#SET-UP ####
#Load libraries 

library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(here)
library(dplyr)
library(ggplot2)
library(ggridges)

#read data files
trawl_catch  <- read.csv(here::here("Processed_data", 
                                    "trawl",
                                    "catch_data",
                                    "clean_data",
                                    "trawl_catch_clean.csv"),
                         head=TRUE)

#isolate species with fork length data 
fork <- subset(trawl_catch, length_type == 'Fork')
fork <- select(fork, c('species', 'length_cm', 'sample_id'))
fork_spp <- select(fork, c('species'))
fork_spp <- distinct(fork_spp)

#Fixing species that have fork length data instead of total length data 

#Species 1: Oncorhynchus tshawytscha ####
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=244&GenusName=Oncorhynchus&SpeciesName=tshawytscha&fc=76
#TL= 1.034FL
fork_ot <- subset(fork, species == 'Oncorhynchus tshawytscha')
min(fork_ot$length_cm) #21.5
max(fork_ot$length_cm) #66.9 

fork_ot$total_length_cm <- 1.034*fork_ot$length_cm


#Species 2: Clupea pallasii ####
#no conversion from FL to TL on fishbase
#convert based on: https://onlinelibrary.wiley.com/doi/full/10.1111/jfb.14105
#LT =1.110LF – 0.323

fork_cp <- subset(fork, species == 'Clupea pallasii')
min(fork_cp$length_cm) #7.6
max(fork_cp$length_cm) #30.8

fork_cp$total_length_cm <- 1.110*fork_cp$length_cm-0.323 

#Species 3:Engraulis mordax ####
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=1664&GenusName=Engraulis&SpeciesName=mordax&fc=454
#LT =1.083LF 

fork_em <- subset(fork, species == 'Engraulis mordax')
min(fork_em$length_cm) #11.3
max(fork_em$length_cm) #16.2

fork_em$total_length_cm <- 1.083*fork_em$length_cm 

#Species 4: Oncorhynchus gorbuscha ####
#convert based on: https://www.fishbase.se/popdyn/LLRelationshipList.php?ID=240&GenusName=Oncorhynchus&SpeciesName=gorbuscha&fc=76
#LT =1.028FL

fork_og <- subset(fork, species == 'Oncorhynchus gorbuscha')
min(fork_og$length_cm) #11.4
max(fork_og$length_cm) #19.9

fork_og$total_length_cm <- 1.028*fork_og$length_cm

#Species 5:Thaleichthys pacificus ####
#not available in fishbase
#convert based on: https://onlinelibrary.wiley.com/doi/full/10.1111/j.0022-1112.2005.00741.x
#this is the conversion for thawed specimens after freezing 
#LT =1.078FL+0.451

fork_tp <- subset(fork, species == 'Thaleichthys pacificus')
min(fork_tp$length_cm) 
fork_tp <- subset(fork_tp, !is.na(length_cm)) #remove NA
min(fork_tp$length_cm) #6.6
max(fork_tp$length_cm) #132


fork_tp$total_length_cm <- 1.078*fork_tp$length_cm + 0.451

#Species 6: Mallotus villosus ####
#no FL to TL available on fishbase 
#convert based on: https://onlinelibrary.wiley.com/doi/full/10.1111/j.0022-1112.2005.00741.x
#this is the conversion for thawed specimens after freezing 
#LT =1.075FL+0.449

fork_mv <- subset(fork, species == 'Mallotus villosus')
min(fork_mv$length_cm) #9.2
max(fork_mv$length_cm) #9.2


fork_mv$total_length_cm <- 1.075*fork_mv$length_cm + 0.449

#Species 7: Oncorhynchus kisutch ####
#no FL to TL available on fishbase 
#convert based on: https://afspubs.onlinelibrary.wiley.com/doi/abs/10.1577/1548-8640%281995%29057%3C0250%3ATLTFLR%3E2.3.CO%3B2
#TL = 1.0822(FL) – 0.0861

fork_ok <- subset(fork, species == 'Oncorhynchus kisutch')
min(fork_ok$length_cm) #13.5
max(fork_ok$length_cm) #66


fork_ok$total_length_cm <- 1.0822*fork_ok$length_cm + 0.0861


#Species 8: Anoplopoma fimbria ####
#on fishbase
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=512&GenusName=Anoplopoma&SpeciesName=fimbria&fc=270
#TL = 1.025(FL) 

fork_af <- subset(fork, species == 'Anoplopoma fimbria')
min(fork_af$length_cm) #41.1
max(fork_af$length_cm) #48


fork_af$total_length_cm <- 1.025*fork_af$length_cm 


#Species 9: Oncorhynchus nerka ####
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=243&GenusName=Oncorhynchus&SpeciesName=nerka&fc=76
##TL = FL/0.923 
fork_on <- subset(fork, species == 'Oncorhynchus nerka')
min(fork_on$length_cm) #NA
fork_on <- subset(fork_on, !is.na(length_cm)) #remove NA
min(fork_on$length_cm) #55.5
max(fork_on$length_cm) #65.8


fork_on$total_length_cm <- (fork_on$length_cm / 0.923)


#Species 10: Allosmerus elongatus ####
#no length-length table on fishbase
#convert based on: https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/348681.pdf
##TL = FL/0.909
fork_ae <- subset(fork, species == 'Allosmerus elongatus')
min(fork_ae$length_cm) #11.3
max(fork_ae$length_cm) #12.3


fork_ae$total_length_cm <- (fork_ae$length_cm / 0.909)

#Species 11: 	Oncorhynchus keta ####
#FL to TL available on fishbase 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=241&GenusName=Oncorhynchus&SpeciesName=keta&fc=76
##TL = 1.017FL
fork_ok2 <- subset(fork, species == 'Oncorhynchus keta')
min(fork_ok2$length_cm) #14.6
max(fork_ok2$length_cm) #21.2


fork_ok2$total_length_cm <- (fork_ok2$length_cm*1.017)

#Species 12: 	Alosa sapidissima ####
#FL to TL available on fishbase 
#convert based on: https://fishbase.se/popdyn/LLRelationshipList.php?ID=1584&GenusName=Alosa&SpeciesName=sapidissima&fc=43
##TL = 1.127FL
fork_as <- subset(fork, species == 'Alosa sapidissima')
min(fork_as$length_cm) #25.5
max(fork_as$length_cm) #19.


fork_as$total_length_cm <- (fork_as$length_cm*1.127)

#COMBINE ALL ####
length<- rbind(fork_ae, fork_af, fork_as, fork_cp, fork_em, fork_ot, fork_mv, fork_og, fork_ok, fork_on, fork_tp, fork_ok2)

#now we add total_length_cm to all species in trawl_catch 
fork <- subset(trawl_catch, length_type == 'Fork')
merge_fork <- merge(fork, length, by=c('sample_id', 'species','length_cm'), all.x=TRUE)

total <- subset(trawl_catch, length_type == 'Total')
total$total_length_cm <- total$length_cm

#combine 
final_df <- rbind(total, merge_fork)

write_csv(length,
          here("Processed_data",
               "traits",
               "length_conversions.csv")) 
