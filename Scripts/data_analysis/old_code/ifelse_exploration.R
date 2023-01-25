#experimenting w/ if else and for 
#lets experiment with for and if statements 
eDNA_species <- eDNA_data$LCT

eDNA_new <- eDNA_data %>%
  select(LCT, set_number) %>%
  summarise(for (species in eDNA_species){
    if (species %in% both){
      eDNA_data$method <- c("both") 
    } else {
      eDNA_data$method <- c("eDNA") 
    }
  }) 

eDNA_new <- eDNA_data %>%
  select(LCT) %>%
  for (species in eDNA_species){
    if (species %in% both){
      eDNA_data$method <- c("both") 
    } else {
      eDNA_data$method <- c("eDNA") 
    }
  }

for (species in eDNA_species){
  if (species %in% both){
    print("both")
  } else {
    print("not both")
  }
} #it works like this 

for (species in eDNA_species){
  if (species %in% both){
    eDNA_data$method <- c("both") 
  } else {
    eDNA_data$method <- c("eDNA") 
  }
  
} #why doesn't it work here? 