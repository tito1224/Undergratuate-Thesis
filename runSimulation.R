library(tidyverse)
library(RMark)
source("./Functions/generatePointCount.R")

# start with helper functions


## create a dataframe to represent all model combinations
## does FullHet include the three way interactions whereas the HetClosed is the 12 models described in Pledger's paper? - nvm 
## actually I believe if we need to model individual heterogeneity (ie use mixture argument) then we use either HetClosed or FullHet
## use FullHet in order to have additive values that include heterogeneity? on page 23 of the program mark appendix, looks like they use the FullHet model  
## useC means detection probability after first capture is different (takes into account trap happy or trap shy effect)
## useTime means detection probability varies across time intervals --> do we assume random variation? 
## useMixture means heterogeneity in detection probability across individuals 
dataVariation = function(useC=FALSE,useTime=FALSE,useMixture=FALSE){
  
}

## assuming we have the data, fit the model, with necessary specifications
## useC means detection probability after first capture is different (takes into account)
fitModel = function(ch,useC=FALSE,useTime=FALSE,useMixture=FALSE){
  
}