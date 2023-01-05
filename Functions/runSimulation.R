library(tidyverse)
library(RMark)
library(stringi)
library(rlang)
source("./Functions/generatePointCount.R")

# start with helper functions


## create a dataframe to represent all model combinations - doing it this way so if we need to make a change to the formula, it only needs to be made once in this section
## I believe if we need to model individual heterogeneity (ie use mixture argument) then we use either HetClosed or FullHet
## HetClosed does not include c as a parameter whereas FullHet includes c (refer to https://github.com/jlaake/RMark/blob/master/RMark/inst/MarkModels.pdf)
## also it seems we can only have two groups for the mixture model? <- nvm mark() has a mixtures argument that we can change to use more groups
generateFormula = function(){
  # create combinations
  lstC = c("c","NA")
  lstTime = c("time","NA")
  lstMixture = c("mixture","NA")
  lstOperation =c("+","*") # whether it's additive or we include interaction
  dfCombinations = expand.grid(lstC,lstTime,lstMixture,lstOperation)
  colnames(dfCombinations) = c("C","Time","Mixture","Operation")
  dfCombinations = mutate(dfCombinations, formula = NA)
  
  # add what the formula should be 
  ## code below creates formulas for Pledger's 12 models (not including partially interactive models between M_t*b*h and M_t+b+h)
  
  # boolean to indicate whether row is an additive model or interactive
  bAddition = which(dfCombinations$Operation=="+")
  bInteraction = which(dfCombinations$Operation=="*")
  dfCombinations[bAddition,"formula"] = paste(dfCombinations[bAddition,"C"],dfCombinations[bAddition,"Time"],dfCombinations[bAddition,"Mixture"],sep = "+")
  dfCombinations[bInteraction,"formula"] = paste(dfCombinations[bInteraction,"C"],dfCombinations[bInteraction,"Time"],dfCombinations[bInteraction,"Mixture"],sep = "*")
  
  # do some cleaning of the string -> remove whitespaces and instances where the formula starts/ends with an operator
  lstRemove = c("NA+","+NA"," ","NA*","*NA")
  dfCombinations$formula = stri_replace_all_fixed(dfCombinations$formula,
                                                  pattern = lstRemove, 
                                                  replacement = rep("",length(lstRemove)),
                                                  vectorize_all = FALSE)
  
 dfCombinations$formula = paste("~",dfCombinations$formula,sep="")
 #dfCombinations = dfCombinations[!duplicated(dfCombinations$formula),]
 dfCombinations[dfCombinations$formula=="~NA","formula"] = "~1"
 
 # add another column to indicate what the model type should be 
 dfCombinations$model = ifelse(dfCombinations$Mixture == "NA","Closed","FullHet")
 #dfCombinations$model = ifelse(dfCombinations$Mixture=="NA","Closed", ifelse(dfCombinations$Mixture!="NA"& dfCombinations$C == "NA","HetClosed","FullHet"))
 
 # revert values in C, Time and Mixture columns to boolean for use in later code
 dfCombinations[,1:3] = ifelse(dfCombinations[,1:3]=="NA",FALSE,TRUE)
 return(dfCombinations)
}

## assuming we have the data, fit the model, with necessary specifications
## use the parameters to determine which formula and model to use
## bC means detection probability after first capture is different (takes into account trap happy or trap shy effect; ie behaviour)
## bTime means detection probability varies across time intervals --> assume random variation? 
## bMixture means heterogeneity in detection probability across individuals (so if we assume A groups, each individual has a probability of belonging to each group. I think rMark uses 2 groups by default)
## bAdditive means we are looking at an additive model (no interactions included). If false, it means we are looking at a model with all interactions in addition to main effects
#Q: use real or beta df? fitModel(dfMark, bC=TRUE,bTime=TRUE, bAdditive=TRUE) vs fitModel(dfMark, bC=TRUE,bTime=TRUE, bAdditive=FALSE)
fitModel = function(ch,bC=FALSE,bTime=FALSE,bMixture=FALSE,bAdditive=TRUE,nMixtures=1){
  
  bAdditive = ifelse(bAdditive==TRUE,"+","*")
  
  # find formula and model type
  dfModel = generateFormula() %>%
    filter(C == bC & Time == bTime & Mixture == bMixture & Operation == bAdditive)
  
  # fit the model 
  pformula = list(formula = eval(parse_expr(dfModel$formula)),share=TRUE)
  model = mark(ch, model = dfModel$model, model.parameters = list(p=pformula),delete=TRUE,output=FALSE,mixtures=nMixtures)
  
  # extract relevant variables - estimate, se, ul, cl, aic
  dfPopulationEstimates = model$results$derived$`N Population Size`
  dfPopulationEstimates$variable = "N"
  intAIC = model$results$AICc
  
  # get estimates of p, c (if applicable), se, lcl & ucl
  estP = model$results$real[,1:4]
  #estP = model$results$beta[,1:4]
  estP = tibble::rownames_to_column(estP, "variable")
  dfEstimates = bind_rows(estP, dfPopulationEstimates)
  dfEstimates$AIC = intAIC
  
  # clean data 
  dfEstimates = filter(dfEstimates,!str_detect(variable,"f0"))
  
  return(dfEstimates)
}

runSingleSimulation = function(N_i,p_1m=0.1,maxMinute=5,alpha=0, seed = NULL,bC=FALSE,bTime=FALSE,bMixture=FALSE,bAdditive=TRUE,nMixtures=1){
  # generate regular data
  #locationID = 1:n_locations
  #N_i = rpois(n_locations,lambda)
  #N_i = rnbinom(n_locations, mu = lambda, size= 1)
  
  # conditional for if population value generated is 0
  #if(N_i ==0){
  #  return(list(NA,NA))
  #}
  
  dfMarkInitial = generateDetects(N_i,p_1m,maxMinute,seed)
  dfMarkInitial = generateErrors(dfMarkInitial,alpha,seed) # if alpha = 0, error data is the same as regular data (checked in previous .Rmd)
  dfMark = detectsToCapHist(dfMarkInitial)[,"ch"] # for now combine all detections regardless of locationID 
  
  # add conditional for if no individuals are detected 
  if(nrow(dfMark)==0){
    return(list(NA,NA))
  }
  
  # fit model
  dfResults = fitModel(dfMark,bC=bC,bTime=bTime,bMixture=bMixture,bAdditive = bAdditive,nMixtures = nMixtures)
  pResults = dfResults[1,]
  
  # if bMixture is used, the first p value is p_i not p_1
  # code below gives us probability of detection if you belong in group 1
  if(bMixture==TRUE){
    pResults = dfResults[2,]
  }
  
  NResults = filter(dfResults, variable == "N")
  # q: programMark has a "misidentification" feature --> explore more?
  tempdf = rbind(pResults,NResults)
    
  # make summarised dataframe
  lstParams = c(p_1m,sum(N_i))
  dfOutput = tibble(trueValues = lstParams)
  dfOutput = cbind(dfOutput,tempdf)
  
  # df to store parameters used
  dfParameters = tibble(p_1m = p_1m,
                        alpha = alpha,
                        maxMinute = maxMinute,
                        N = sum(N_i),
                        bC=FALSE,
                        bTime=FALSE,
                        bMixture=FALSE,
                        bAdditive=TRUE)
  return(list(dfOutput, dfParameters,dfMarkInitial))
}

# function to automate outputs of the simulation
# lstNi should ideally be generated using rnbinom() or rpois but I think it would make more sense to generate those numbers outside of these functions
runSimulation = function(nRuns = 2, lstNi = c(10,20), lstP = c(0.1,0.5), lstAlpha = c(0,0.3), lstMaxMin = c(10),seed=NULL,bC=FALSE,bTime=FALSE,bMixture=FALSE,bAdditive=TRUE,nMixtures=1){
  
  # initialize variables
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  dfFinalEst =  matrix(0,0,0)
  dfFinalParams = matrix(0,0,0) # so that if we see any weird numbers, we can use the runSingleSimulation() function to investigate
  dfFinalHist = data.frame() # store capture history
  dfCombinations = expand.grid(lstNi,lstP, lstAlpha, lstMaxMin)
  colnames(dfCombinations) = c("Ni","P","Alpha","MaxMin")
  
  # counter to record simulation number
  simulationNumber = 1
  
  # combination counter -> keep track of how many times the combination has run
  combinationNumber = 1
  
  # grab parameter values
  for(row in 1:nrow(dfCombinations)){
    temp_Ni = dfCombinations[row,"Ni"]
    temp_p = dfCombinations[row,"P"]
    temp_alpha = dfCombinations[row,"Alpha"]
    temp_min = dfCombinations[row,"MaxMin"]
    
    # run the simulation multiple times for each parameter combo 
    for(sim in 1:nRuns){
      dfTemp = runSingleSimulation(N_i = temp_Ni,p_1m=temp_p,maxMinute=temp_min,alpha=temp_alpha,bC=bC,bTime=bTime,bMixture=bMixture,bAdditive = bAdditive,nMixtures = nMixtures,seed=NULL)
      dfTempEst = dfTemp[[1]]
      dfTempParams = dfTemp[[2]]
      dfHist = dfTemp[[3]]
      
      # add conditional
      if(is.null(nrow(dfTempEst))){
        next()
      }
      
      # add extra variables to track simulation number
      dfTempEst$combinationNumber = combinationNumber
      dfHist$combinationNumber = combinationNumber
      
      dfTempEst$simulationNumber = simulationNumber
      dfTempParams$simulationNumber = simulationNumber
      dfHist$simulationNumber = simulationNumber
      
      dfHist$seed = seed
      dfTempParams$seed = seed
      
      dfFinalEst = rbind(dfFinalEst,dfTempEst)
      dfFinalParams = rbind(dfFinalParams,dfTempParams)
      dfFinalHist = rbind(dfFinalHist,dfHist)
      simulationNumber = simulationNumber+1
    }
    # update combination counter
    combinationNumber=combinationNumber+1
    
  }
  
  # merge dataframes
  dfFinal = left_join(dfFinalEst, dfFinalParams, by="simulationNumber")
  return(list(dfFinal, dfFinalHist)) 
}

calculateStatistics = function(nRuns = 2,lstNi = c(10,20),lstP = c(0.1,0.5), lstAlpha = c(0,0.3), lstMaxMin = c(10),seed=NULL,bC=FALSE,bTime=FALSE,bMixture=FALSE,bAdditive=TRUE,nMixtures=1){
  # gather simulation results
  results = runSimulation(nRuns = nRuns,lstNi = lstNi, lstP = lstP, lstAlpha = lstAlpha, lstMaxMin = lstMaxMin,seed=seed,bC=bC,bTime=bTime,bMixture=bMixture,bAdditive = bAdditive,nMixtures = nMixtures)
  simData = results[[1]]
  dfHist = results[[2]]
  
  # find summary stats
  # notes: coverage probability isthe proportion of confidence intervals that capture the true population parameter 
  simData$bCoverage = ifelse(simData$trueValue >= simData$lcl & simData$trueValue <= simData$ucl,1,0)
  simData$squaredError = (simData$estimate - simData$trueValues)^2
  simData$width = simData$ucl - simData$lcl
  
  dfSummaryStats = simData %>%
    filter(variable == "N")%>% # only focus on N? or also use p??
    group_by(p_1m,alpha,N,combinationNumber)%>%
    summarise(AvgNhat = mean(estimate),
              AvgNhatSE= mean(se),
              sdNhat = sd(estimate),
              biasNhat = AvgNhat - N,
              mse = mean(squaredError),
              bias_SE_Nhat = AvgNhatSE - sdNhat,
              coverage = sum(bCoverage)/n(),
              avgWidth = mean(width),
              avgAIC = mean(AIC))
  dfSummaryStats = dfSummaryStats[!duplicated(dfSummaryStats),]
  return(list(dfSummaryStats, simData, dfHist))
}