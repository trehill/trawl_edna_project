#Jaccard Tables + Plots
#goal: visualize jaccard components and format values into a table

library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(rempsyc)


data3 <- read.csv(here::here("Processed_data",
                            "diversity",
                            "diversity_indices_all.csv"),
                 head=TRUE)

#table w/ all sets 

data3$set_number <- as.numeric(data3$set_number)
data3 <- data3 %>% arrange(set_number)
data3$set_number <- as.character(data3$set_number)

colnames(data3) <- c('site','Jaccards','Jaccards Turnover', 'Jaccards Nestedness')
data3 <- select(data3, c('site','Jaccards','Jaccards Turnover', 'Jaccards Nestedness'))

my_table <- nice_table(
  data3[1:16, ], 
  title = c("Table 2", "Diversity Indices Dissimilarities"), 
  note = c("The data was calculated from adapted methods from Baselga & Leprieur (2015)"))

my_table


#Make bar plot showing proportion nestedness + proportion turnover 

data <- select(data3, c('site', 'Jaccards Turnover', 'Jaccards Nestedness'))
colnames(data) <- c('site','Turnover', 'Nestedness')
data_long <- gather(data, dissimilarity, measurement, c('Turnover'):c('Nestedness'), factor_key=TRUE)

#order in decreasing nestedness, increasing turnover
data_long$site <- factor(data_long$site,levels = c("1", "4", "5", "11",'16','15','6','10','12','9','13','14','8','3','2','7'))

plot <- ggplot(data_long, aes(fill=dissimilarity, y=measurement, x=site)) + 
  geom_bar(position = 'fill', stat="identity") +
  scale_fill_manual(values=c("#00AFBB", "#132B43")) +
  xlab("site") + ylab("Proportion of Jaccards Dissimilarity") +
  theme_classic()
plot


ggsave("./Outputs/diversity/jacccomponents_all.png", 
       plot = plot,
       width = 10, height = 5, units = "in")
