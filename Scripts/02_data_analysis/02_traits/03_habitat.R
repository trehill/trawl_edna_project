#Habitat 
#goal: visualize the habitats patterns of detections using alluvia plots 

#install.packages("ggalluvial")

library(tidyr)
library(dplyr)
library(here)
library(ggalluvial)



beta_div <- read.csv(here::here("Processed_data", #should be ASV by sample
                                "datasets",
                                "detections_all.csv"),
                     head=TRUE)

trait_data <- read.csv(here::here("Processed_data", 
                                  "traits",
                                  "traitdatabase.csv"),
                       head=TRUE)


#we need to isolate only detection + habitat
data <- merge(beta_div, trait_data, by="LCT", all.x= TRUE)
df <- select(data, c('gamma_detection_method','habitat'))
df <- subset(df, !is.na(gamma_detection_method))

#change reef associated variable 
df <- data.frame(lapply(data, function(x) {
  gsub("reef_associated", "reef associated", x) 
  
}))


#need to give habitat levels
df$habitat = factor(df$habitat, levels = c('reef associated', 'benthopelagic', 'pelagic','bathypelagic', 'bathydemersal','demersal'))

#need to give method levels
df$gamma_detection_method = factor(df$gamma_detection_method, levels = c('only trawl', 'both eDNA/trawl', 'only eDNA'))


df <- subset(df, !is.na(habitat)) #remove NA

df <- df %>% arrange(desc(habitat))

df$freq <- 1

#plot 
df <- df %>% rename(
  method = gamma_detection_method,
  )



plot <- ggplot(data = df,
       aes(axis1 = habitat, axis2 = method, y = freq)) +
  geom_alluvium(aes(fill = method),
                curve_type = "cubic") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Survey", "Response"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values = c("#00AFBB","#5491cf","#FCC442") ) +
  theme_void()

plot


ggsave("./Outputs/traits/habitat_alluvia.png", 
       plot = plot,
       width = 6, height = 7, units = "in")

