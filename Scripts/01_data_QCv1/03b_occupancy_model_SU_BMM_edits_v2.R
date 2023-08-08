#03b_occupancy_model 

#Packages####

#download Jags
#install.packages("R2jags")

#use this of you haven't installed many of these packages
#list.of.packages <- c("vegan","plotrix","MASS","data.table", "gdata","lattice","plyr","dplyr", "lme4", "arm", "gridExtra", "ggplot2", "eeptools", "taxize", "lmtest", "Biostrings", "tidyr", "googlesheets", "wesanderson", "drc", "zoo","unmarked","jagsUI", "data.table","vegan","digest")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

libs=c("vegan","plotrix","MASS","data.table", "gdata","lattice","plyr","dplyr", "lme4", "arm", "gridExtra", "ggplot2", "eeptools", "taxize", "lmtest", "Biostrings", "tidyr", "googlesheets", "wesanderson", "drc", "zoo","unmarked","jagsUI", "data.table","vegan","digest","tidyverse")
lapply(libs, require, character.only = TRUE)  #"gsubfn"


#Functions####
# This function is later used for printing simulation results on the screen
printsummres<-function(thetahat,thename="estimated parameter"){
  cat(paste0("\n",thename,": mean = ",round(mean(thetahat),2),
             ", 2.5% = ",round(quantile(thetahat,prob=0.025),2),
             ", 97.5% = ",round(quantile(thetahat,prob=0.975),2)))
}

ProbOcc=function(x, psi=psi, p11=p11, p10=p10, K=K){
  (psi*(p11^x)*(1-p11)^(K-x)) / ((psi*(p11^x)*(1-p11)^(K-x))+(((1-psi)*(p10^x))*((1-p10)^(K-x))))
}
cumProb=function(x) 1-prod(1-x)  #define cumulative prob function
sitesFun<-function(x, Nreplicates=2){rep(1:x, each=Nreplicates)}



#Model####
####Bayesian version
#set p10_max and select data
p10_max=0.1								

# Run first this part once to create the file with the JAGS model####
sink("./Scripts/occupancy_modelling/royle_link/RoyleLink_prior.txt")
cat("model {
								    # Priors
								    psi ~ dunif(0,1)
								    p11 ~ dunif(0.01,1)
								    p10 ~ dunif(0.001, p10_max)
								    
								    # Likelihood 
								    for (i in 1:S){
								    z[i] ~ dbern(psi)
								    p[i] <- z[i]*p11 + (1-z[i])*p10
								    for (j in 1:K){
								    Y[i,j] ~ dbern(p[i])
								    }
								    }
								    } ",fill=TRUE)
sink()
#The Model####
model_Bayesian <- function(datalist, COLUMN, nOTUs=length(datalist), S=18, K=2, doprint=FALSE,p10_max=0.1,
                           ni=5000,nt=2,nc=10,nb=1000,myparallel=TRUE) {   
  psihat<-p11hat<-p10hat<-rep(nOTUs)
  modelSummaries<-list()
  hh<-datalist[[COLUMN]]
  # fit the model    
  jags.inits <-function()(list(psi=runif(1,0.05,0.95),p11=runif(1, 0.01,1),p10=runif(1,0.001,p10_max)))
  jags.data  <-list(Y=hh,S=S[[1]],K=K,p10_max=p10_max)
  jags.params<-c("psi","p11","p10")
  model<-jags(data = jags.data, inits = jags.inits, parameters.to.save= jags.params, 
              model.file= "RoyleLink_prior.txt", n.thin= nt, n.chains= nc, 
              n.iter= ni, n.burnin = nb, parallel=myparallel)
  #  jpeg(paste0(format(Sys.time(), "%H_%M_%S"),"_ModelParamsPlotDEMO.jpg"))
  #  plot(model)
  #  dev.off()
  #  plot(model)
  
  psihat[COLUMN] <- model$summary["psi","50%"]
  p11hat[COLUMN] <- model$summary["p11","50%"]
  p10hat[COLUMN] <- model$summary["p10","50%"]    
  modelSummaries[[COLUMN]]<-model$summary
  
  
  #  if (doprint){
  #    printsummres(psihat,thename="estimated psi")
  #    printsummres(p11hat,thename="estimated p11")
  #    printsummres(p10hat,thename="estimated p10")
  #  }
  #saveRDS(modelSummaries, paste0(format(Sys.time(), "%H_%M_%S"),"_ModelSummaries_DEMO.rds"))
  BayesResults<-list(psihat=psihat,p11hat=p11hat,p10hat=p10hat,modelSummaries=modelSummaries)
  return(BayesResults)
  return(plot(model))
}

#Run the Model ####

#Run for all ASVs, output psi, p11 and p10
#set datalist and ASV names in model, can load these from directory

datalist = ASVlist

#use 1:10 for trials

  List <- list()
  for(i in 1:423){
    model <- model_Bayesian(datalist, i)
    List[[length(List)+1]] = model$modelSummaries[[i]][1:3]
  }
  
  out <- as.data.frame(List) 
  colnames(out) = ASVs[c(1:423)]
  out <- as.data.frame(t(out))
  setnames(out, c("V1", "V2", "V3"), c("psi", "p11", "p10"))
  

#calculate probability of occupancy for each ASV from psi, p11 and p10

  out$PoO.0 <-
    (out$psi*(out$p11^0)*(1-out$p11)^(2-0)) / ((out$psi*(out$p11^0)*(1-out$p11)^(2-0))+(((1-out$psi)*(out$p10^0))*((1-out$p10)^(2-0))))
  out$PoO.1 <-
    (out$psi*(out$p11^1)*(1-out$p11)^(2-1)) / ((out$psi*(out$p11^1)*(1-out$p11)^(2-1))+(((1-out$psi)*(out$p10^1))*((1-out$p10)^(2-1))))
  out$PoO.2 <-
    (out$psi*(out$p11^2)*(1-out$p11)^(2-2)) / ((out$psi*(out$p11^2)*(1-out$p11)^(2-2))+(((1-out$psi)*(out$p10^2))*((1-out$p10)^(2-2))))

 write.csv(out, "./Scripts/occupancy_modelling/royle_link/scratch/occProb_royallink_u.csv")
 saveRDS(out, "./Scripts/occupancy_modelling/royle_link/scratch/occProb_royallink_u.rds")



  
  