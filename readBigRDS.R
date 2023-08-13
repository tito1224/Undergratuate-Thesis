
library(tidyverse)
library(gtools)
library(data.table)
library(matrixStats)
library(RMark)
library(stringi)
library(rlang)
#library(gt)
#library(gtExtras)
#library(webshot2)
library(knitr)
library(readxl)

source("./Functions/generatePointCount.R")
source("./Functions/runSimulationFunctions.R")

readBigRDS = function(strPath = "./FinalOutput/Closure"){
  setwd(strPath)
  dataFinalTrial = list.files(pattern = ".rds") # load rds files
  dfClosure = map_df(dataFinalTrial, read_rds)
  return(dfClosure)
}

results = readBigRDS(strPath ="./FinalOutput/Closure" )
outfile = paste0("closure_final_",".csv") # note that the Closure folder is in the FinalOutput folder
saveRDS(results,outfile)
