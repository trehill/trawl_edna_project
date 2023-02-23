#Diversity Analysis 
#Author: Ben MM, Tessa Rehill 
#goal: conduct analysis of nestedness + turnover using Jaccards indices 

#install.packages("lubridate")
#install.packages("lme4")
#install.packages("scales")
#install.packages("AICcmodavg")
#install.packages("MASS")
#install.packages("MuMIn")
#install.packages("performance")
#install.packages("geepack")
#install.packages("gee")
#install.packages("rempsyc")

#Set-Up ####
library(tidyverse)
library(lubridate)
library(here)
library(lme4)
library(scales)
library(vegan)
library(AICcmodavg)
library(MASS)
library(MuMIn)
library(performance)
library(RColorBrewer)
library(geepack)
library(gee)
library(dplyr)
library(rempsyc)

#SET UP ####
#long data to calculate richness
long <- read.csv(here::here("Processed_data",
                            "datasets",
                            "detections.csv"),
                        head=TRUE)

long <- distinct(long)

meta <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "metadata", 
                            "clean_data",
                            "trawl_metadata.csv"),
                 head=TRUE)

a1 <- long %>%
  dplyr::select(c("set_number", "LCT", "pabs_trawl", "p_abs_eDNA")) %>%
  rename("trawl" = "pabs_trawl","eDNA" = "p_abs_eDNA")  %>%
  pivot_longer(!c("set_number", "LCT"), names_to = "method", values_to = "pa") %>%
  mutate(id = paste(set_number, method, sep = "_")) %>%
  dplyr::select(c("id", "pa", "LCT")) %>%
  pivot_wider(names_from = "LCT", values_from = "pa") %>%
  column_to_rownames("id") %>%  
  replace(is.na(.), 0)  

Jac <- vegdist(a1, "jaccard")
#from Baselga https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12388
Jac_turn <- designdist(a1, "2 * pmin(b,c) / (a + 2 * pmin(b,c))", abcd = T)
Jac_nest <- designdist(a1, "((pmax(b,c)-pmin(b,c)) / (a+b+c)) * (a / (a + (2 * pmin(b,c))))", abcd = T)
Sor_turn <- designdist(a1, "pmin(b,c) / (a + pmin(b,c))", abcd = T)
Sor_nest <- designdist(a1, "(pmax(b,c)-pmin(b,c)) / (2*a+b+c) * a / (a + pmin(b,c))", abcd = T)

j <- as.matrix(Jac) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Jac" = "V1")

jt <- as.matrix(Jac_turn) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Jac_turn" = "V1")

jn <- as.matrix(Jac_nest) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Jac_nest" = "V1")

sn <- as.matrix(Sor_nest) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Sor_nest" = "V1")

st <- as.matrix(Sor_turn) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Sor_turn" = "V1")

#put all data frames into list
df_list <- list(j, jt, jn, st, sn)
#merge all data frames in list
beta_metrics <- df_list %>% 
  reduce(full_join, by='set_number')


#it looks like set #7 and #5 share the exact same members (have no dissimilarity since they = 1.00)
data <- beta_metrics

write_csv(data,
          here("Processed_data",
               "diversity",
               "diversity_indices.csv")) 


data2 <- data
#plot nestedness and turnover
b1 <- data %>%
  dplyr::select(c("set_number", "Jac_nest", "Jac_turn")) %>%
  pivot_longer(!set_number, names_to = "metric", values_to = "value")

b2<- merge(data, meta, by=c('set_number'))

b3<- merge(b1, meta, by=c('set_number'))

#plot with set as color index
b1 <- b1 %>% rename(set = set_number)
#add 0 before set numbers 

betadiff <-
ggplot(b1, aes(x = metric, y = value, group = set, color=set)) +
  geom_point() +
  geom_line() +
  ylab("dissimilarity") +
  xlab("Jaccards component") + 
  theme_classic()+ 
  scale_x_discrete(labels=c("Jac_nest" = "nestedness", "Jac_turn" = "turnover"))
betadiff

ggsave("./Outputs/diversity/jaccards_set.png", 
       plot = betadiff,
       width = 5, height = 5, units = "in")


#plot with region as color index

b3 <- b3 %>% rename(region = leg)

betadiff <-
  ggplot(b3, aes(x = metric, y = value, group = set_number, color=region)) +
  geom_point() +
  geom_line() +
  ylab("dissimilarity") +
  xlab("Jaccards component") + 
  theme_classic()+ 
  scale_x_discrete(labels=c("Jac_nest" = "nestedness", "Jac_turn" = "turnover"))
betadiff

ggsave("./Outputs/diversity/jaccards_region.png", 
       plot = betadiff,
       width = 5, height = 5, units = "in")

#FOR ALL SETS ####
long <- read.csv(here::here("Processed_data",
                            "datasets",
                            "detections_all.csv"),
                 head=TRUE)

long <- distinct(long)

meta <- read.csv(here::here("Processed_data", 
                            "trawl",
                            "metadata", 
                            "clean_data",
                            "trawl_metadata.csv"),
                 head=TRUE)

a1 <- long %>%
  dplyr::select(c("set_number", "LCT", "pabs_trawl", "p_abs_eDNA")) %>%
  rename("trawl" = "pabs_trawl","eDNA" = "p_abs_eDNA")  %>%
  pivot_longer(!c("set_number", "LCT"), names_to = "method", values_to = "pa") %>%
  mutate(id = paste(set_number, method, sep = "_")) %>%
  dplyr::select(c("id", "pa", "LCT")) %>%
  pivot_wider(names_from = "LCT", values_from = "pa") %>%
  column_to_rownames("id") %>%  
  replace(is.na(.), 0)  

Jac <- vegdist(a1, "jaccard")
#from Baselga https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12388
Jac_turn <- designdist(a1, "2 * pmin(b,c) / (a + 2 * pmin(b,c))", abcd = T)
Jac_nest <- designdist(a1, "((pmax(b,c)-pmin(b,c)) / (a+b+c)) * (a / (a + (2 * pmin(b,c))))", abcd = T)
Sor_turn <- designdist(a1, "pmin(b,c) / (a + pmin(b,c))", abcd = T)
Sor_nest <- designdist(a1, "(pmax(b,c)-pmin(b,c)) / (2*a+b+c) * a / (a + pmin(b,c))", abcd = T)

j <- as.matrix(Jac) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Jac" = "V1")

jt <- as.matrix(Jac_turn) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Jac_turn" = "V1")

jn <- as.matrix(Jac_nest) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Jac_nest" = "V1")

sn <- as.matrix(Sor_nest) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Sor_nest" = "V1")

st <- as.matrix(Sor_turn) %>%
  as.data.frame() %>%
  rownames_to_column("A") %>%
  pivot_longer(!A, names_to = "B", values_to = "V1") %>%
  separate(A, c("lnkA", "methodA"), sep = "_") %>%
  separate(B, c("lnkB", "methodB"), sep = "_") %>%
  mutate(common_lnk = if_else(lnkA == lnkB,"y", "n")) %>%
  mutate(common_method = if_else(methodA == methodB,"y", "n")) %>%
  filter(common_lnk == "y" & common_method == "n") %>%
  dplyr::select(c("lnkA", "V1")) %>%
  distinct() %>%
  rename("set_number" = "lnkA") %>%
  rename("Sor_turn" = "V1")

#put all data frames into list
df_list <- list(j, jt, jn, st, sn)
#merge all data frames in list
beta_metrics <- df_list %>% 
  reduce(full_join, by='set_number')


#it looks like set #7 and #5 share the exact same members (have no dissimilarity since they = 1.00)
data <- beta_metrics

write_csv(data,
          here("Processed_data",
               "diversity",
               "diversity_indices_all.csv")) 

data3 <- data

#plot nestedness and turnover
b1 <- data %>%
  dplyr::select(c("set_number", "Jac_nest", "Jac_turn")) %>%
  pivot_longer(!set_number, names_to = "metric", values_to = "value")

b2<- merge(data, meta, by=c('set_number'))

b3<- merge(b1, meta, by=c('set_number'))

#plot with set as color index
b1 <- b1 %>% rename(set = set_number)

betadiff <-
  ggplot(b1, aes(x = metric, y = value, group = set, color=set)) +
  geom_point() +
  geom_line() +
  ylab("dissimilarity") +
  xlab("Jaccards component") + 
  theme_classic()+ 
  scale_x_discrete(labels=c("Jac_nest" = "nestedness", "Jac_turn" = "turnover"))
betadiff

ggsave("./Outputs/diversity/jaccards_set_all.png", 
       plot = betadiff,
       width = 5, height = 5, units = "in")


#plot with region as color index

b3 <- b3 %>% rename(region = leg)

betadiff <-
  ggplot(b3, aes(x = metric, y = value, group = set_number, color=region)) +
  geom_point() +
  geom_line() +
  ylab("dissimilarity") +
  xlab("Jaccards component") + 
  theme_classic()+ 
  scale_x_discrete(labels=c("Jac_nest" = "nestedness", "Jac_turn" = "turnover"))
betadiff

ggsave("./Outputs/diversity/jaccards_region_all.png", 
       plot = betadiff,
       width = 5, height = 5, units = "in")


#plot as a linear line nestedness vs. turnover 
plot <- ggplot(beta_metrics,aes(Jac_nest, Jac_turn, color = set_number)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) + 
  labs(x = "Nestedness", y = "Turnover") +
  theme_classic()

plot 

ggsave("./Outputs/diversity/nestednessvsturnover.png", 
       plot = plot,
       width = 5, height = 5, units = "in")

#can add diagrams for each point manually

#TABLES ####
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

flextable::save_as_docx(my_table, path = "nice_tablehere.docx")

#table w/ all sets 
colnames(data3) <- c('site','Jaccards','Jaccards Turnover', 'Jaccards Nestedness')

data3$set_number <- as.numeric(data3$set_number)
data3 <- data3 %>% arrange(set_number)
data3$set_number <- as.character(data3$set_number)

colnames(data3) <- c('site','Jaccards','Jaccards Turnover', 'Jaccards Nestedness')

my_table <- nice_table(
  data3[1:16, ], 
  title = c("Table 1", "Diversity Indices Dissimilarities (all sets)"), 
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
