library(tidyverse)
library(RMark)
library(stringi)
library(rlang)
source("./Functions/generatePointCount.R")
source("./Functions/runSimulationFunctions.R")

# add simulation wrapper function by Dr. Bonner to help keep track of what is 
# run on the super computers

simulation_wrapper = function(params){
  
  ## extract parameter values
  nRuns = params$nRuns
  lstNi = params$lstNi
  lstP = params$lstP
  lstAlpha = params$lstAlpha
  lstMaxMin = params$lstMaxMin
  lstFormula=params$lstFormula 
  lstMixtures=params$lstMixtures
  lstSeed=params$lstSeed
  strModel=params$strModel
  
  # run the simulation
  results = calculateStatistics(nRuns = nRuns[id], lstNi = lstNi[id], lstP = lstP[id], 
                      lstAlpha = lstAlpha[id], lstMaxMin = lstMaxMin[id],
                      lstFormula=lstFormula[id],lstMixtures=lstMixtures[id],
                      seed=lstSeed[id],strModel=strModel[id])
  # return results
  return(results)
}