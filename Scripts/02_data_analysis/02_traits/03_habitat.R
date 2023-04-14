#Habitat 
#goal: visualize the habitats patterns of detections using alluvia plots 

#SET-UP ####
#install.packages("ggalluvial")

#read libraries 
library(tidyr)
library(dplyr)
library(here)
library(ggalluvial)

#read data
beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "detections_all.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)


#we need to isolate only detection + habitat
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE) #merge data
df <- select(data, c('gamma_detection_method','habitat')) #select relevant columns
df <- subset(df, !is.na(gamma_detection_method)) #remove NA values

#change reef associated variable (no more underscore)
df <- data.frame(lapply(data, function(x) {
  gsub("reef_associated", "reef associated", x) }))

#give habitats 'level's in order of increasing depth 
df$habitat = factor(df$habitat, levels = c('reef associated', 'benthopelagic', 'pelagic','bathypelagic', 'bathydemersal','demersal'))

#give methods levels so eDNA on bottom and trawl on surface (both between)
df$gamma_detection_method = factor(df$gamma_detection_method, levels = c('only trawl', 'both eDNA/trawl', 'only eDNA'))

df <- subset(df, !is.na(habitat)) #remove NA

df <- df %>% arrange(desc(habitat)) #arange habitat categories in line with established levels

df$freq <- 1 #give each observation a numeric frequency of 1 (1 observation)

df <- df %>% rename(method = gamma_detection_method,) #rename column 

#plot 
plot <- ggplot(data = df,
       aes(axis1 = habitat, axis2 = method, y = freq)) +
  geom_alluvium(aes(fill = method),
                curve_type = "cubic", alpha =.9) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = c("#5491cf","#00AFBB","#FCC442") ) +
  theme_void()

plot


ggsave("./Outputs/traits/habitat_alluvia.png", 
       plot = plot,
       width = 6.5, height = 7, units = "in")

#what would it look like for species instead of observation 
#columns of interest are habitat, method and frequency 

new <- select(df, c('habitat', 'method', 'freq', 'LCT'))
new <- distinct(new)

plot <- ggplot(data = new,
               aes(axis1 = habitat, axis2 = method, y = freq)) +
  geom_alluvium(aes(fill = method),
                curve_type = "cubic", alpha =1) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = c("#5491cf", "#00AFBB","#FCC442") ) +
  theme_void()

plot

ggsave("./Outputs/traits/habitat_alluvia_spp.png", 
       plot = plot,
       width = 6, height = 7, units = "in")


