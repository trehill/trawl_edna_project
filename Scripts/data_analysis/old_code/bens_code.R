trawl_meta <- read.csv(here::here("Processed_data", #should be ASV by sample
                                  "trawl",
                                  "metadata",
                                  "clean_data",
                                  "trawl_metadata.csv"),
                       head=TRUE)

eDNA_meta_12su <- read.csv(here::here("Processed_data", #should be ASV by sample
                                      "eDNA",
                                      "12s",
                                      "12s_u",
                                      "asv",
                                      "matrix",
                                      "clean_data",
                                      "data12Su_asvmatrix_metadata_nc.csv"),
                           head=TRUE)
eDNA_meta_12se <- read.csv(here::here("Processed_data", #should be ASV by sample
                                      "eDNA",
                                      "12s",
                                      "12s_e",
                                      "asv",
                                      "matrix",
                                      "clean_data",
                                      "data12Se_asvmatrix_metadata_nc.csv"),
                           head=TRUE)

#Dataframe cleaning + curation 
#find out which set numbers have greater than 50m between eDNA sample and tral 
trawl <- select(trawl_meta, c("set_number", "depth_mean"))
eDNA <- select(eDNA_meta_12se, c("set_number", "depth"))
depths <- merge(trawl, eDNA, by="set_number")

#figure out all set_numbers that have more than 50m differences 

#make new column which is difference between two values 
depths$difference <- abs(depths$depth_mean - depths$depth)



valid_sets <- subset(depths, difference <= 50,
                  select=c(set_number, difference))

valid_set_numbers <- unique(valid_sets$set_number)

unique(valid_set_numbers)

####BENs CODE

ASVbysite <- readRDS("Data/2021_09_20/derived_data/data12se_asvmatrix_lor_12s.rds")
common_taxa <- read_csv("Data/2021_09_20/derived_data/common_taxa.csv")

taxa <- readRDS("Data/2021_09_20/derived_data/taxa_ASVs_resolved.rds")

sitesurvey_data <- readRDS("Data/2021_09_20/derived_data/sitesurvey_data.rds")

#taxa$lca_taxon <- ifelse(is.na(taxa$family), taxa$order,
#                         ifelse(is.na(taxa$genus), taxa$family,
#                                ifelse(is.na(taxa$species), taxa$genus, taxa$species)))

#colnames(taxa) <- gsub("X.", replacement = "", names(taxa)) #remove "X." from column names
#colnames(taxa)[1] <- "ASV"
taxa <- taxa %>%
  rename("ASV" = "query")

# transpose ASVbysite and add taxonomy (at any level)
t1 <- as.data.frame(t(ASVbysite))

t2 <- rownames_to_column(t1, var = "ASV") %>% 
  merge(taxa, ., by = "ASV", all.y = T) %>% # lose 5 ASVs that don't have assignment
  drop_na(kingdom) %>% #some things made it through occupancy models but were not assigned any taxonomy
  merge(common_taxa[c("lca_taxon", "group", "level")],., by = "lca_taxon", all.y = T)
t2$lca_taxon <- if_else(is.na(t2$group), t2$lca_taxon, t2$group)


g1 <- t2 %>%
  filter(class == "Actinopterygii" | class == "Holocephali") %>%
  filter(!is.na(genus)) %>%
  .[c(1, 22:103)] %>% 
  group_by(lca_taxon) %>% 
  summarise_all(sum) 

#summarize for each lca_taxon (remove duplication)
t3 <-  t2 %>% 
  .[c(1, 22:103)] %>% 
  group_by(lca_taxon) %>% 
  summarise_all(sum) 

s1 <- t3 %>%
  column_to_rownames("lca_taxon") %>%
  t(.) %>%
  as.data.frame()
saveRDS(s1, "Data/2021_09_20/derived_data/lcabysitesurvey_matrix_alltaxa.rds")

s2 <- g1 %>%
  column_to_rownames("lca_taxon") %>%
  t(.) %>%
  as.data.frame()
saveRDS(s2, "Data/2021_09_20/derived_data/lcabysitesurvey_matrix.rds")

#summarize taxonomy table so there's only one entry for each taxa
p1 <- taxa
p1$ASV <- NULL
p2 <- p1[!duplicated(p1), ]
row.names(p2) <- NULL

tax1 <- t2 %>%
  filter(class == "Actinopterygii" | class == "Holocephali") %>%
  filter(!is.na(genus)) %>%
  .[c(1:3, 6:11)] 
tax1$lca_taxon <- if_else(tax1$lca_taxon == "Mola sp. B", "Mola1", tax1$lca_taxon)

tax1$gen <- if_else(tax1$level == "family", tax1$group, tax1$genus)
tax1$genus <- if_else(is.na(tax1$gen), tax1$genus, tax1$gen)
tax2 <- tax1[c(1,4:9)] %>%
  distinct()

saveRDS(p2, "Data/2021_09_20/derived_data/taxa_eDNA.rds")
saveRDS(tax2, "Data/2021_09_20/derived_data/taxa_eDNA.rds")

#merge taxa and taxa table
t4 <- merge(p2, t3, by = "lca_taxon", all.y = T)

#calculate total ASVs across all surveys
t5 <- t4 %>% mutate(ASVsum = select(., c(19:100)) %>% rowSums(na.rm = TRUE)) %>% relocate(ASVsum, .after = subspecies_id)

#stacked bar plots and pie charts ####
#keeping all taxa in for interests sake, could filter as is done below for SADs

family_summary <- t5 %>%
  group_by(family) %>%
  summarize(sum = sum(ASVsum))

#set colour palette
colourCount = length(unique(family_summary$family))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))

#ggplot(family_summary, aes(x="", y=sum, fill=family)) +
#  geom_col(stat="identity", width=1) +
#  coord_polar(theta = "y") +
#  geom_col(color = "black") +              #doesn't look so good yet
#  theme_void() +
#  theme(legend.position = "none") +
#  geom_text(aes(label = family, x = 1.8),
#            position = position_stack(vjust = 0.5)) 
ggplot(family_summary, aes(x = "", y = sum, fill = family)) +
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_col(color = "black") +
  theme_classic()+
  ylab("read count (all sites)") +
  xlab(NULL)+
  ggtitle("Family Summary - All sites")

#summarize by order
order_summary <- t5 %>%
  group_by(order) %>%
  summarize(sum = sum(ASVsum))

colourCount = length(unique(order_summary$order))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))

ggplot(order_summary, aes(x = "", y = sum, fill = order)) +
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_col(color = "black") +
  theme_classic()+
  ylab("read count (all sites)") +
  xlab(NULL) +
  ggtitle("Order Summary - All sites")

#summarize by species
species_summary <- t5 %>%
  group_by(species) %>%
  summarize(sum = sum(ASVsum)) 

colourCount = length(unique(species_summary$species))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))

ggplot(species_summary, aes(x = "", y = sum, fill = species)) +
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values = getPalette(colourCount)) +
  geom_col(color = "black") +
  theme_classic() +
  ylab("read count (all sites)") +
  xlab(NULL)

#SADs ####
#filter out non-targets
t5 <- t4 %>%
  filter(class == "Holocephali" | class == "Actinopterygii")
t6 <- t5 %>%  .[c(1,19:100)] %>% column_to_rownames(.,"lca_taxon") 

t7 <- as.data.frame(t(t6))

spe_long <- gather(t7)

s5 <- spe_long
s5$abund_rel <- s5$value / sum(s5$value) # relative abundance of each observation
s5$rank <- 130 - rank(s5$abund_rel, ties.method = "random")
s6 <- s5 %>%
  group_by(key) %>%
  summarise(abund_mean = mean(abund_rel), sd = sd(abund_rel))
s6$rank <- 130 - rank(s6$abund_mean, ties.method = "random")
s6$error <- qnorm(0.95)*s6$sd/sqrt(130)
s6$CI_low <- s6$abund_mean - s6$error
s6$CI_high <- s6$abund_mean + s6$error

ggplot(s6, aes(x = rank, y = abund_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ifelse(CI_low < 0, 0.000000001, CI_low), ymax = CI_high)) +
  scale_y_log10(limits = c(0.000000001, 0.1)) +
  theme_classic() +
  ylab("Mean relative read abundance") +
  xlab("Species rank") +
  geom_text(aes(label=key),hjust= -0.35,vjust=0.5, size = 4, angle = 90) 


