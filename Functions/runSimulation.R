# set MarkPath (only for running on supercomputer)
MarkPath = "/home/oadebajo/bin"

## Load packages
library(tidyverse)
library(RMark)
library(stringi)
library(rlang)

## Load source code
source("./Functions/generatePointCount.R")
source("./Functions/runSimulationFunctions.R")
source("./Functions/simulation_wrapper.R")

## Read command line arguments
args = commandArgs(trailingOnly=TRUE)

## Set job number
id = as.integer(args[1]) 

## list parameters and make a dataframe out of it as inputs
## this requires 8G
# nRuns = 500
# lstNi = c(50)
# lstP = c(0.05,0.1,0.2,0.3)
# lstAlpha =c(0,0.05,0.1,0.2)
# lstMaxMin = c(5)
# lstFormula = c("~1")
# lstMixtures = c(1)
# lstSeed = c("NULL")
# strModel = "Closed"

nRuns = 500
lstNi = c(10,20)
lstP = c(0.1,0.2,0.3,0.4,0.5)
lstAlpha =c(0.05,0.1,0.2,0.3)
lstMaxMin = c(5,10)
lstFormula = c("~1")
lstMixtures = c(1)
lstSeed = c("NULL")
strModel = "Closed"

params = expand.grid(nRuns,lstNi,lstP, lstAlpha, lstMaxMin,lstFormula,lstMixtures,lstSeed,strModel)
colnames(params) = c("nRuns","lstNi","lstP","lstAlpha","lstMaxMin","lstFormula","lstMixtures","lstSeed","strModel")
params$lstFormula = as.character(params$lstFormula) # for some reason this column turns into a factor variable?
params$lstSeed = as.character(params$lstSeed) # for some reason i need to wrap this with as.character()
params$Scenario = 1:nrow(params)

# params = mutate(params, Scenario = row_number()) %>%
#   crossing(Rep = 1:nRuns)

## Run simulation with parameters from id row of parameter matrix
results = simulation_wrapper(params[id,])

# return results
## Save that number
outfile = paste0("Output/output_scenario",id,"_",".rds")
saveRDS(results,outfile)
