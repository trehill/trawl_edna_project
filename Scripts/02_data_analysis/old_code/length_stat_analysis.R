#Length Analysis 
#goal: statistical analysis of length distributions 

library(lattice)
library(plyr)
library(ggplot2)

dat <- read.csv(here::here("Processed_data", #does not include 'trawl individuals' 
                                "traits",
                                "length_2.csv"),
                     head=TRUE)

dat1 <- read.csv(here::here("Processed_data", #includes trawl individuals, only eDNA/trawl
                           "traits",
                           "length.csv"), 
                head=TRUE)



#Is there a true difference in distributions between indv. trawl + all trawl(detected by eDNA + trawl?) max. length? ####

#dataset
#extract ind. trawl, only trawl + both data from dat1 dataset 
df <- subset(dat1, detection %in% c('trawl individuals','only trawl','both eDNA/trawl'))  
df <- df[!is.na(df$length_cm),] #remove NA 

#rename only trawl + both eDNA/trawl to one common name trawl max 
df <- data.frame(lapply(df, function(x) {
  gsub("only trawl", "trawl max", x) 
  
}))

df <- data.frame(lapply(df, function(x) {
  gsub("both eDNA/trawl", "trawl max", x) 
  
}))
#change length to numeric 
df$length_cm <- as.numeric(df$length_cm)

#plot distributions
histogram(~ length_cm | detection, data = df, col = "gray60", layout = c(1, 2),
          xlab = list("Species Length (cm) "),
          ylab = list("Percentage of total"),
          scales = list(y = list(alternating = F)),
          strip = strip.custom(factor.levels = c("trawl individuals", "trawl max")))

qqplot(df$length_cm[df$detection == "trawl individuals"],
       df$length_cm[df$detection == "trawl max"])
abline(a = 0, b = 1, lty = 3)

#Two-sample Kolmogorov-Smirnov test
ks.test(df$length_cm[df$detection == "trawl individuals"],
        df$length_cm[df$detection == "trawl max"])

#Wilcox test
wilcox.test(length_cm ~ detection, data = df)

#T-test
t.test(length_cm ~ detection, data = df)


# Overlaid density plots with means

#plot again w/ distributions + mean
cdat <- ddply(df, "detection", summarise, length.mean=mean(length_cm))
cdat

plot <- ggplot(df, aes(x=length_cm, colour=detection)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=length.mean,  colour=detection),
             linetype="dashed", size=1) + 
  theme_classic()
plot

ggsave("./Outputs/traits/indvsmax.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#I want to know if there is a true difference in distribution between only eDNA + only trawl (only max length)####

#subset dat1 for only eDNA and only trawl 
df <- subset(dat1, detection %in% c('only trawl','only eDNA'))  
df <- df[!is.na(df$length_cm),] #remove NA

#plot distributions
histogram(~ length_cm | detection, data = df, col = "gray60", layout = c(1, 2),
          xlab = list("Maximum Species Length (cm) "),
          ylab = list("Percentage of total"),
          scales = list(y = list(alternating = F)),
          strip = strip.custom(factor.levels = c("only eDNA", "only trawl")))

qqplot(df$length_cm[df$detection == "only eDNA"],
       df$length_cm[df$detection == "only trawl"])
abline(a = 0, b = 1, lty = 3)

#Two-sample Kolmogorov-Smirnov test
ks.test(df$length_cm[df$detection == "only eDNA"],
        df$length_cm[df$detection == "only trawl"])

#Wilcox test
wilcox.test(length_cm ~ detection, data = df)

#T-test
t.test(length_cm ~ detection, data = df)

#plot again w/ distributions + mean
cdat <- ddply(df, "detection", summarise, length.mean=mean(length_cm))
cdat

plot <- ggplot(df, aes(x=length_cm, colour=detection)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=length.mean,  colour=detection),
             linetype="dashed", size=1) + 
  theme_classic()
plot

ggsave("./Outputs/traits/onlyeDNAtrawl.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

#I want to know if there is a true difference between all eDNA + all trawl (only max length) ####
histogram(~ max_length_cm |gamma_detection_method, data = dat, col = "gray60", layout = c(1, 2),
          xlab = list("Maximum Species Length (cm)"),
          ylab = list("Percentage of total"),
          scales = list(y = list(alternating = F)),
          strip = strip.custom(factor.levels = c("eDNA", "trawl")))

qqplot(dat$max_length_cm[dat$gamma_detection_method == "trawl"],
       dat$max_length_cm[dat$gamma_detection_method == "eDNA"])
abline(a = 0, b = 1, lty = 3)

#Two-sample Kolmogorov-Smirnov test
ks.test(dat$max_length_cm[dat$gamma_detection_method == "trawl"],
        dat$max_length_cm[dat$gamma_detection_method == "eDNA"])

#Wilcox test
wilcox.test(max_length_cm ~ gamma_detection_method, data = dat)

#T-test
t.test(max_length_cm ~ gamma_detection_method, data = dat)

#plot again w/ distributions + mean
cdat <- ddply(dat, "gamma_detection_method", summarise, length.mean=mean(max_length_cm))
cdat

plot <- ggplot(dat, aes(x=max_length_cm, colour=gamma_detection_method)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=length.mean,  colour=gamma_detection_method),
             linetype="dashed", size=1) + 
  theme_classic()
plot


ggsave("./Outputs/traits/alltrawledna.png", 
       plot = plot,
       width = 10, height = 6, units = "in")

