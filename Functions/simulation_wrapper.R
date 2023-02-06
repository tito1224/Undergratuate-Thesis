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
  ## test if i can at least generate data
  ###dfMarkInitial = generateDetects(lstNi,lstP,lstMaxMin)
  ###dfMarkInitial = generateErrors(dfMarkInitial,lstAlpha) # if alpha = 0, error data is the same as regular data (checked in previous .Rmd)
  ###dfMark = detectsToCapHist(dfMarkInitial)[,"ch"] # for now combine all detections regardless of locationID, issue in original code is the [id] in this part
  
  results = calculateStatistics(nRuns = nRuns, lstNi = lstNi, lstP = lstP,
                      lstAlpha = lstAlpha, lstMaxMin = lstMaxMin,
                      lstFormula=lstFormula,lstMixtures=lstMixtures,
                      seed=as.character(lstSeed),strModel=strModel)
  # return results
  #results = dfMark
  return(results)
}