
#SET UP 
#install.packages('circlize')
library(here)
library(circlize)
library(dplyr)

beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "detections.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)


#we need to isolate only detection + habitat
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)
df <- select(data, c('gamma_detection_method','habitat'))
df <- subset(df, !is.na(gamma_detection_method))
df <- subset(df, !is.na(habitat))

#need to give habitat levels
df$habitat = factor(df$habitat, levels = c('demersal', 'bathydemersal', 'bathypelagic','pelagic', 'benthopelagic','reef_associated'))

df <- df %>% arrange(desc(habitat))

#write_csv(df,
#          here("Processed_data",
#              "traits",
#             "habitat.csv")) 

n <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "habitat.csv"),
                       head=TRUE)


m <- n 

#Color the links

#convert the table to a martix
data <- as.matrix(m)

#assign color to each group of strains
#assign color to each group of strains
col = c('demersal'="#202020", 'bathydemersal'="#606060" , 'bathypelagic'="#808080",'pelagic'="#A0A0A0" ,
        'benthopelagic'="#C0C0C0" , 'reef_associated'="#E0E0E0" , 
        'only trawl'="#0D838B" , 'both eDNA/trawl'="#00AFBB", 'only eDNA'="#FCC442")

#"#4F76C4" purple-ish

#create a chord diagram but without labeling 
chordDiagram(data, grid.col = col, annotationTrack = "grid", preAllocateTracks = 1)

#add the labels and axis
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  #print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  
  #print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.length = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)


#Plot eDNA and trawl (no both category) ####
beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "detections.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)

#merge identification data w/ trait data 
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)

#change 'both' to eDNA or trawl alone

#subset for 'both' data only 
both_to_eDNA <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to eDNA 
both_to_eDNA <- data.frame(lapply(both_to_eDNA, function(x) {
  gsub("both eDNA/trawl", "eDNA", x) 
  
}))

#same for trawl 
both_to_trawl <- subset(data, gamma_detection_method == 'both eDNA/trawl')

#change all both to trawl 
both_to_trawl <- data.frame(lapply(both_to_trawl, function(x) {
  gsub("both eDNA/trawl", "trawl", x) 
  
}))

#merge together 
both_fixed <- rbind(both_to_eDNA, both_to_trawl)

#merge back with our original data
without_both <- subset(data, gamma_detection_method != 'both eDNA/trawl')
data_new <- rbind(both_fixed, without_both)

#fix only... to simple trawl or eDNA 
data <- data.frame(lapply(data_new, function(x) {
  gsub("only eDNA", "eDNA", x) 
  
}))

data <- data.frame(lapply(data, function(x) {
  gsub("only trawl", "trawl", x) 
  
}))

df <- select(data, c('gamma_detection_method','habitat'))
df <- subset(df, !is.na(gamma_detection_method))
df <- subset(df, !is.na(habitat))
df$habitat = factor(df$habitat, levels = c('demersal', 'bathydemersal', 'bathypelagic','pelagic', 'benthopelagic','reef_associated'))

df <- df %>% arrange(desc(habitat))

#write_csv(df,
#          here("Processed_data",
#               "traits",
#              "habitat2.csv")) 

n <- read.csv(here::here("Processed_data", 
                         "traits",
                         "habitat2.csv"),
              head=TRUE)


m <- n 

#Color the links

#convert the table to a martix
data <- as.matrix(m)

#assign color to each group of strains
#assign color to each group of strains
col = c('demersal'="#202020", 'bathydemersal'="#606060" , 'bathypelagic'="#808080",'pelagic'="#A0A0A0" ,
        'benthopelagic'="#C0C0C0" , 'reef_associated'="#E0E0E0" , 
         'trawl'="#00AFBB", 'eDNA'="#FCC442")





#create a chord diagram but without labeling 
chordDiagram(data, grid.col = col, annotationTrack = "grid", preAllocateTracks = 1)

#add the labels and axis
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  #print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  
  #print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.length = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)



#Now let's try but instead of magnitude being abundance just per species  ####

beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "detections.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)


#we need to isolate only detection + habitat
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)
df <- select(data, c('gamma_detection_method','habitat', 'LCT'))
df <- distinct(df)
df <- subset(df, !is.na(habitat))
df <- subset(df, !is.na(gamma_detection_method))
df <- select(data, c('gamma_detection_method','habitat'))
df$habitat = factor(df$habitat, levels = c('demersal', 'bathydemersal', 'bathypelagic','pelagic', 'benthopelagic','reef_associated'))

df <- df %>% arrange(desc(habitat))


#write_csv(df,
#         here("Processed_data",
#               "traits",
#               "habitat3.csv")) 

n <- read.csv(here::here("Processed_data", 
                         "traits",
                         "habitat3.csv"),
              head=TRUE)


m <- n 


#convert the table to a martix
data <- as.matrix(m)

#assign color to each group of strains
col = c('demersal'="#202020", 'bathydemersal'="#606060" , 'bathypelagic'="#808080",'pelagic'="#A0A0A0" ,
        'benthopelagic'="#C0C0C0" , 'reef_associated'="#E0E0E0" , 
        'only trawl'="#0D838B" , 'both eDNA/trawl'="#00AFBB", 'only eDNA'="#FCC442")




#create a chord diagram but without labeling 
chordDiagram(data, grid.col = col, annotationTrack = "grid", preAllocateTracks = 1)

#add the labels and axis
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  #print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.6)
  
  #print axis
  circos.axis(h = "top", labels.cex = 0.5, major.tick.length = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)

