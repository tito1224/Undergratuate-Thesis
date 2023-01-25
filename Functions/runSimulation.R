## Load packages
library(tidyverse)
library(RMark)
library(stringi)
library(rlang)

## Load source code
source("./Functions/generatePointCount.R")
source("./Functions/runSimulationFunctions.R")

## Read command line arguments
args <- commandArgs(trailingOnly=TRUE)

## Set job number
id <- as.integer(args[1])

## list parameters and make a dataframe out of it as inputs
nRuns = 2
lstNi = c(20,50)
lstP = c(0.1,0.2)
lstAlpha =c(0.1,0.3)
lstMaxMin = c(5,10)
lstFormula = c("~1")
lstMixtures = c(1)
lstseed = c(NULL)
strModel = "Closed"

params = expand.grid(nRuns,lstNi,lstP, lstAlpha, lstMaxMin,lstFormula,lstMixtures,lstSeed,strModel)
colnames(params) = c(nRuns,"lstNi","lstP","lstAlpha","lstMaxMin","lstFormula","lstMixtures","lstSeed","strModel")
params$Formula = as.character(params$Formula) # for some reason this column turns into a factor variable?
# params = mutate(params, Scenario = row_number()) %>%
#   crossing(Rep = 1:nRuns)

## Run simulation with parameters from id row of parameter matrix
results <- simulation_wrapper(params[id,])

## Save that number
outfile <- paste0("Output/output_",params$Scenario[id],"_",params$Rep[id],".rds")
saveRDS(r,outfile)
