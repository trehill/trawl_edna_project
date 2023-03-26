#Jaccard Tables + Plots
#goal: visualize jaccard components and format values into a table

library(tidyr)
library(tidyverse)
library(here)
library(dplyr)
library(rempsyc)

data2 <- read.csv(here::here("Processed_data",
                            "diversity",
                            "diversity_indices.csv"),
                 head=TRUE)

data3 <- read.csv(here::here("Processed_data",
                            "diversity",
                            "diversity_indices_all.csv"),
                 head=TRUE)


#table w/ selected sets
data2<- select(data2, c('set_number','Jac','Jac_turn','Jac_nest')) #select columns of interest 

data2$set_number <- as.numeric(data2$set_number)
data2 <- data2 %>% arrange(set_number)
data2$set_number <- as.character(data2$set_number)

#change column names 
colnames(data2) <- c('site','Jaccards','Jaccards Turnover', 'Jaccards Nestedness')

my_table <- nice_table(
  data2[1:15, ], 
  title = c("Table 1", "Diversity Dissimilarities"), 
  note = c("These values are calculated based on adapted methods from Baselga & Leprieur (2015)"))


my_table


#table w/ all sets 

data3$set_number <- as.numeric(data3$set_number)
data3 <- data3 %>% arrange(set_number)
data3$set_number <- as.character(data3$set_number)

colnames(data3) <- c('site','Jaccards','Jaccards Turnover', 'Jaccards Nestedness')
data3 <- select(data3, c('site','Jaccards','Jaccards Turnover', 'Jaccards Nestedness'))

my_table <- nice_table(
  data3[1:16, ], 
  title = c("Table 1", "Diversity Indices Dissimilarities"), 
  note = c("The data was calculated from adapted methods from Baselga & Leprieur (2015)"))

my_table


#Make bar plot showing proportion nestedness + proportion turnover 
#make data2 in long format
data <- select(data2, c('site', 'Jaccards Turnover', 'Jaccards Nestedness'))
colnames(data) <- c('site','Turnover', 'Nestedness')
data_long <- gather(data, dissimilarity, measurement, c('Turnover'):c('Nestedness'), factor_key=TRUE)

#order in decreasing nestedness, increasing turnover
data_long$site <- factor(data_long$site,levels = c("1", "4", "5", "11",'16','10','12','3','9','13','14','8','2','7'))

plot <- ggplot(data_long, aes(fill=dissimilarity, y=measurement, x=site)) + 
  geom_bar(position = 'fill', stat="identity") +
  scale_fill_manual(values=c("#00AFBB", "#132B43")) +
  xlab("site") + ylab("Proportion of Jaccards Dissimilarity") +
  theme_classic()
plot


ggsave("./Outputs/diversity/nestednessturnovercomponents.png", 
       plot = plot,
       width = 10, height = 5, units = "in")

#Now make one for all sets 
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


ggsave("./Outputs/diversity/nestednessturnovercomponents_all.png", 
       plot = plot,
       width = 10, height = 5, units = "in")
