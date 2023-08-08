library(tidyverse)

#####plotting Royal Link Model#######

out <- readRDS("./Scripts/occupancy_modelling/royle_link/scratch/occProb_royallink_u.rds")

#or
t5 <- out    #from OccupancyModel.R

ASVcountNo0s$det_type <- as.character(ifelse(ASVcountNo0s == 1, "dataset_singleton", "multiple_observations")) # create ASVcountNo0s in wrangling.R

# make the data
po0 <- as.data.frame(t5$PoO.0)
colnames(po0)[1] <- "prob" 
po0$pos_reps <- 0
rownames(po0) <- rownames(t5)
po0 <- cbind(ASVcountNo0s,po0)

po1 <- as.data.frame(t5$PoO.1)
colnames(po1)[1] <- "prob" 
po1$pos_reps <- 1
rownames(po1) <- rownames(t5)
po1 <- cbind(ASVcountNo0s,po1)

po2 <- as.data.frame(t5$PoO.2)
colnames(po2)[1] <- "prob" 
po2$pos_reps <- 2
rownames(po2) <- rownames(t5)
po2 <- cbind(ASVcountNo0s,po2)

po3 <- as.data.frame(t5$PoO.3)
colnames(po3)[1] <- "prob" 
po3$pos_reps <- 3
rownames(po3) <- rownames(t5)
po3 <- cbind(ASVcountNo0s,po3)


#join and rename
reps_occprob <- rbind(po0, po1, po2, po3)
reps_occprob$det_type <- as.character(reps_occprob$det_type)
colnames(reps_occprob)[1] <- "det_count"               #rename column



# plot the data####
# each ASV if found in 0, 1, 2 or 3 PCR replicates 763 ASVs x 4 = 3052
p1 <- ggplot(reps_occprob, aes(factor(pos_reps), prob)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  geom_violin(scale = "width", trim = F, adjust = 2, size = 0.5, fill = "grey") +
  geom_jitter(aes(colour = det_type)) +
#  geom_jitter(aes(colour = det_count)) +
#  scale_color_gradientn(colours = rainbow(5)) +
  theme_classic( base_size = 12) +
  xlab("number of positive detection in 3 PCR replicates (763 ASVs)") +
  ylab("occupancy probability") +
  ggtitle("Occupancy Probability by replicate PCR - RL model") + 
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  annotate("text", x=1, y = 0.835, label = "80% probability threshold") 

p1

r1 <- reps_occprob
r1$pos_reps <- as.character(r1$pos_reps)

p2 <- ggplot(r1, aes(prob, color = pos_reps)) +
  geom_histogram(binwidth = 0.01, fill = "white",alpha=0.5, position="identity") +
  theme_classic() +
  xlab("occupancy probability") +
  ylab("number of detections (n = ###)") +
  geom_vline(xintercept = 0.8, linetype = "dashed") +
  annotate("text", x=0.785, y = 500, angle = 90, label = "80% probability threshold") +
  ggtitle("distribution of occupancy probabilities - mifishE")

p2

