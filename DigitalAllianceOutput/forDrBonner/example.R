# load libraries & source functions

library(tidyverse)
library(gtools)
library(data.table)
library(matrixStats)
library(RMark)
library(stringi)
library(rlang)

source("./Functions/generatePointCount.R")
source("./Functions/runSimulationFunctions.R")

# read from .rds into dataframes
# combinationNumber represents the scenario and simulationNumber represents the nth run
dataTrialFive = list.files(pattern = ".rds")
dataTrialFive = mixedsort(dataTrialFive)%>% # use mixedsort so it appears in ascending order 
  map(readRDS)

dfSummarisedFinal= data.frame() # summary stats table
dfEstimatesFinal = data.frame() # data frame of all estimates for every simulation
dfHistoryFinal = data.frame() # data frame of capture history

for (item in dataTrialFive){
  tempSummarisedResults = item[[1]] # retrieve the summary stats
  tempCalculations = item[[2]]
  tempHistory= item[[3]]
  
  
  dfSummarisedFinal = rbind(dfSummarisedFinal,tempSummarisedResults)
  dfEstimatesFinal = rbind(dfEstimatesFinal, tempCalculations)
  dfHistoryFinal = rbind(dfHistoryFinal, tempHistory)
}


# find values where mark encounters do not match real unique encounters in data
dfMisMatch = dfEstimatesFinal %>%
  filter(variable == "N") %>%
  filter(MarkEncounters != DataEncounters)
nMisMatch = nrow(dfMisMatch)
nEstimates = nrow(dfEstimatesFinal %>%
                    filter(variable == "N"))
print(nMisMatch)
print(nMisMatch/nEstimates)

# find biggest differences
dfMisMatch$Difference = dfMisMatch$MarkEncounters - dfMisMatch$DataEncounters
head(dfMisMatch%>%
       filter(!is.na(estimate))%>%
       arrange(desc(Difference)))%>%
  select(trueValues, variable, estimate, MarkEncounters, DataEncounters, combinationNumber, simulationNumber, Difference)

############### code to test cases ######################
test = dfHistoryFinal %>%
  filter(combinationNumber==51 & simulationNumber == 337) 
print(test) # print capture history
print(nrow(unique(test[,"ch"]))) # unique encounter histories

ch = test[,"ch"]
fitModel(ch,scenarioSimNum = "40") # fit the model 
# I have a parameter called scenarioSimNum that will give a prefix to the file name; 
# details of the function can be found in the /Functions/runSimulationFunctions.R script
# here I just gave it a random number -> but when it is being run on the super computer, it uses the job id, along with the simulation number 
# to create the scenarioSimNum number 

############## example of generating data and gathering results ####################################
# for example if i want to generate my own data and get all results I can do the following:
simulateData = calculateStatistics(nRuns = 3, lstNi = c(10), lstP = c(0.2), lstAlpha = c(0.1), lstMaxMin = c(5))
simulateDataSummary = simulateData[[1]] # grab summarized data
simulateDataEstimates = simulateData[[2]] # grab estimates
simulateDataHistory = simulateData[[3]] # grab detection history 

# here you will see markfiles named scenario_1_1, scenario_1_2 and scenario_1_3 because there is only one scenario and there are 3 runs
# if I added something like lstNi =c(10,20) then I would have scenario_1_1, scenario_1_2, scenario_1_3 and scenario_2_1, scenario_2_2 and scenario_2_3
# to represent cases when N= 10 and N = 20
