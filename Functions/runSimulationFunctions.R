library(tidyverse)
library(RMark)
library(stringi)
library(rlang)
source("./Functions/generatePointCount.R")

# start with helper functions


## create a dataframe to represent all model combinations - doing it this way so if we need to make a change to the formula, it only needs to be made once in this section and then this can be referenced
## I believe if we need to model individual heterogeneity (ie use mixture argument) then we use either HetClosed or FullHet
## HetClosed does not include c as a parameter whereas FullHet includes c (refer to https://github.com/jlaake/RMark/blob/master/RMark/inst/MarkModels.pdf)
## however I can't seem to make the HetClosed model work?? For example:
### pformula = list(formula =~time+mixture,share=TRUE)
### model = mark(dfMark, model = "HetClosed", model.parameters = list(p=pformula),delete=TRUE,output=FALSE,mixtures=1)
### model$results
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
#Q: use real or beta df? fitModel(dfMark, bC=TRUE,bTime=TRUE, bAdditive=TRUE) vs fitModel(dfMark, bC=TRUE,bTime=TRUE, bAdditive=FALSE)
fitModel = function(ch,strFormula="~1",strModel="Huggins",nMixtures=1,scenarioSimNum="40"){
  print("starting fit model")
  # fit the model 
  pformula = list(formula = eval(parse_expr(strFormula)),share=TRUE)
  model = mark(ch, model = strModel, model.parameters = list(p=pformula),prefix=scenarioSimNum,delete=TRUE,output=FALSE,mixtures=nMixtures)
  # change delete= TRUE to delete = FALSE when running on super computer!!!
  # find the number of unique encounter histories in the data
  markEncounterHistory = model$results$deviance.df + 2 # double check that this is how df is calculated (# unique histories - 2)
  dataEncounterHistory = nrow(unique(ch))
  
  # extract relevant variables - estimate, se, ul, cl, aic
  dfPopulationEstimates = model$results$derived$`N Population Size`
  dfPopulationEstimates$variable = "N"
  intAIC = model$results$AICc
  
  # get estimates of p, c (if applicable), se, lcl & ucl
  estP = model$results$real[,1:4]
  #estP = model$results$beta[,1:4]
  estP = tibble::rownames_to_column(as.data.frame(estP), "variable")
  dfEstimates = bind_rows(estP, dfPopulationEstimates)
  dfEstimates$AIC = intAIC
  dfEstimates$MarkEncounters = markEncounterHistory
  dfEstimates$DataEncounters = dataEncounterHistory
  
  # clean data 
  dfEstimates = filter(dfEstimates,!str_detect(variable,"f0"))
  
  return(dfEstimates)
}

runSingleSimulation = function(N_i,p_1m=0.1,maxMinute=5,alpha=0,strFormula="~1",strModel="Closed",nMixtures=1,seed = NULL,scenarioSimNum="40"){
  # generate regular data
  #locationID = 1:n_locations
  #N_i = rpois(n_locations,lambda)
  #N_i = rnbinom(n_locations, mu = lambda, size= 1)
  
  # conditional for if population value generated is 0
  #if(N_i ==0){
  #  return(list(NA,NA))
  #}
  
  # need to have NULL input as a string
  
  # remember that Ni can be a list!
  dfMarkInitial = generateDetects(N_i,p_1m,maxMinute,seed)
  dfMarkInitial = generateErrors(dfMarkInitial,alpha,seed) # if alpha = 0, error data is the same as regular data (checked in previous .Rmd)
  dfMark = detectsToCapHist(dfMarkInitial)[,"ch"] # for now combine all detections regardless of locationID 
  dfHistOutput = detectsToCapHist(dfMarkInitial)
  
  # add conditional for if no individuals are detected - doing this to verify that no individuals were detected after the split
  if(nrow(dfHistOutput)==0){
    dfHistOutput = detectsToCapHist(dfMarkInitial,keepZeros = TRUE)
  }
  
  # fit model
  # add tryCatch statement so it keeps running even if the model doesn't fit -> code -200 will show in the dataset to indicate this
  tryCatch({
    dfResults = fitModel(dfMark,strFormula,strModel,nMixtures = nMixtures,scenarioSimNum=scenarioSimNum)
  },error=function(e){} )
  
  # conditional statement for when fitModel doesn't run (probably because detection data is too sparse!)
  if(!exists("dfResults")){
  dfResults = tibble(variable = c("p g1 t1","N"),
                       estimate = c(-200,-200),
                       se = c(NA,NA),
                       lcl=c(NA,NA),
                       ucl = c(NA,NA),
                       AIC = c(NA,NA),
                       MarkEncounters = c(0,0),
                       DataEncounters = c(0,0))
  }
  
  # conditional statement to deal with situations where the fitmodel runs, has no ouput... idek how that's possible! :(
  if(ncol(dfResults)==1){
    dfResults = tibble(variable = c("p g1 t1","N"),
                       estimate = c(-100,-100),
                       se = c(NA,NA),
                       lcl=c(NA,NA),
                       ucl = c(NA,NA),
                       AIC = c(NA,NA),
                       MarkEncounters = c(0,0),
                       DataEncounters = c(0,0))
  }
  
  
  # check for a mismatch in encounter histories
  # if((dfResults$MarkEncounters[1] != dfResults$DataEncounters[1])){
  #   print("mis matched encounter histories")
  #   dfResults$estimate = -300
  # }
  
  #print(dfResults)
  pResults = dfResults[1,]
  
  # if bMixture is used, the first p value is p_i not p_1
  # code below gives us probability of detection if you belong in group 1
  if(strModel!="Huggins"){
    if(nrow(dfResults)==1){
      pResults = dfResults[1,]
    } else {
      pResults = dfResults[2,] 
    }
   
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
                        strFormula = strFormula, 
                        strModel = strModel)
  print("single simulation success")
  return(list(dfOutput, dfParameters,dfHistOutput))
}

# function to automate outputs of the simulation
# lstNi should ideally be generated using rnbinom() or rpois but I think it would make more sense to generate those numbers outside of these functions
runSimulation = function(nRuns = 1, lstNi = c(10,20), lstP = c(0.1,0.5), lstAlpha = c(0,0.3), lstMaxMin = c(10),lstFormula=c("~1"),lstMixtures=c(1),seed=NULL,strModel="Huggins",nScenario=NULL){
  
  # initialize variables
  
  if(!is.null(seed)){ # need to make it nested -> can't evaluate is.null() with another variable within the function...
    if(seed!="NULL"){
      set.seed(seed)
      print("set seed") 
    }
  }
  
  dfFinalEst =  data.frame() # store estimates
  dfFinalParams = data.frame() # so that if we see any weird numbers, we can use the runSingleSimulation() function to investigate
  dfFinalHist = data.frame() # store capture history
  dfCombinations = expand.grid(lstNi,lstP, lstAlpha, lstMaxMin,lstFormula,lstMixtures)
  colnames(dfCombinations) = c("Ni","P","Alpha","MaxMin","Formula","nMixtures")
  dfCombinations$Formula = as.character(dfCombinations$Formula) # for some reason this column turns into a factor variable?
  
  # counter to record simulation number
  simulationNumber = 1
  
  # combination counter -> keep track of how many times the combination has run
  
  if(!is.null(nScenario)){
    combinationNumber = nScenario
  } else {
    combinationNumber = 1 
  }
  
  # grab parameter values
  for(row in 1:nrow(dfCombinations)){
    temp_Ni = dfCombinations[row,"Ni"]
    temp_p = dfCombinations[row,"P"]
    temp_alpha = dfCombinations[row,"Alpha"]
    temp_min = dfCombinations[row,"MaxMin"]
    temp_formula = dfCombinations[row,"Formula"]
    temp_nMixtures = dfCombinations[row,"nMixtures"]
    
    # run the simulation multiple times for each parameter combo 
    for(sim in 1:nRuns){
      # make a unique id for the scenario_simulation number to name the output file
      # prevents confusion of output when mark is being run in parallel 
      scenarioSimNum = paste("scenario",combinationNumber,"_",sim,"_", sep = "")
      
      #specifically set seed to NULL here because I don't want the result of running a single simulation to be the same each time. The seed argument in the runSimulation() function is to save the final result
      dfTemp = runSingleSimulation(N_i = temp_Ni,p_1m=temp_p,maxMinute=temp_min,alpha=temp_alpha,strFormula=temp_formula,strModel=strModel,nMixtures=temp_nMixtures,seed=NULL, scenarioSimNum = scenarioSimNum)
      dfTempEst = dfTemp[[1]] # dataframe of estimates
      dfTempParams = dfTemp[[2]] # dataframe of parameters used 
      dfHist = dfTemp[[3]] # dataframe of count history
      
      # add conditional
      #if(is.null(nrow(dfTempEst))){
      #  next()
      #}
      
      # add extra variables to track simulation number
      dfTempEst$combinationNumber = combinationNumber
      dfHist$combinationNumber = combinationNumber
      dfTempParams$combinationNumber = combinationNumber
      
      dfTempEst$simulationNumber = sim
      dfTempParams$simulationNumber = sim
      dfHist$simulationNumber = sim
      
      dfHist$seed = seed
      dfTempParams$seed = seed
      
      print(data.frame(dfTempEst))
      print(data.frame(dfTempParams))
      print(data.frame(dfHist))
      
      dfFinalEst = rbind(dfFinalEst,dfTempEst)
      dfFinalParams = rbind(dfFinalParams,dfTempParams)
      dfFinalHist = rbind(dfFinalHist,dfHist)
      simulationNumber = simulationNumber+1
      
      # add a sleep timer for 5 seconds -> maybe this will help mark run a little better
      #Sys.sleep(5)
    }
    # update combination counter
    combinationNumber=combinationNumber+1
    
  }
  
  # merge dataframes
  dfFinal = left_join(dfFinalEst, dfFinalParams, by=c("combinationNumber","simulationNumber")) 
  return(list(dfFinal, dfFinalHist,dfFinalEst)) 
}

calculateStatistics = function(nRuns = 1, lstNi = c(10,20), lstP = c(0.1,0.5), lstAlpha = c(0,0.3), lstMaxMin = c(10),lstFormula=c("~1"),lstMixtures=c(1),seed=NULL,strModel="Closed",nScenario=NULL){
  # gather simulation results
  results = runSimulation(nRuns = nRuns,lstNi = lstNi, lstP = lstP, lstAlpha = lstAlpha, lstMaxMin = lstMaxMin,lstFormula = lstFormula, lstMixtures = lstMixtures, seed = seed, strModel = strModel,nScenario=nScenario)
  simData = results[[1]]
  dfHist = results[[2]]
  
  # store original data
  #simDataTrue = simData
  simDataTrue = results[[3]]
  
  # filter out cases that did not run and weird estimates
  simData = simData %>%
    filter(estimate>=0)%>%
    filter(estimate < 1000) #%>%
    #filter(MarkEncounters==DataEncounters)
  
  
  # find summary stats
  # notes: coverage probability is the proportion of confidence intervals that capture the true population parameter 
  simData$bCoverage = ifelse(simData$trueValue >= simData$lcl & simData$trueValue <= simData$ucl,1,0)
  simData$squaredError = (simData$estimate - simData$trueValues)^2
  simData$width = simData$ucl - simData$lcl
  
  dfSummaryStats = simData %>%
    filter(variable == "N")%>% # only focus on N? or also use p??
    group_by(p_1m,alpha,N,maxMinute,strFormula,combinationNumber)%>%
    summarise(AvgNhat = mean(estimate,na.rm=TRUE),
              AvgNhatSE= mean(se,na.rm=TRUE),
              sdNhat = sd(estimate,na.rm=TRUE),
              #biasNhat = AvgNhat - N,
              mse = mean(squaredError,na.rm=TRUE),
              bias_SE_Nhat = AvgNhatSE - sdNhat,
              coverage = sum(bCoverage,na.rm=TRUE)/sum(!is.na(estimate)), # changed this from n() to sum(!is.na()) because I have some NA values for my estimates
              avgWidth = mean(width,na.rm=TRUE),
              avgAIC = mean(AIC,na.rm=TRUE))
  dfSummaryStats = dfSummaryStats[!duplicated(dfSummaryStats),]
  return(list(dfSummaryStats, simDataTrue, dfHist))
}

####################### CLOSURE FUNCTIONS #####################################


# helper function to detect time of first and last occurrence of an observation
# bFirst = TRUE means detect the first occurrence of capture
detectMin = function(row, bFirst = T){
  # split count history into vector 
  
  noCH = which(names(row)=="ch")
  str_ch = strsplit(row[noCH],"")$ch # make this more dynamic instead of hardcoding row[3]
  
  # grab first occurrence of capture history 
  if (bFirst == TRUE){
    val = match(TRUE, str_ch == "1")
  } else { # grab last occurence of capture history 
    noBcounts = which(names(row)=="bCounts")
    if(row[noBcounts]=="=1"){ # row[1] is the bCounts column
      val = 0
    } else {
      revRow= rev(str_ch) # reverse the row (last to first)
      nColumns= length(revRow)
      val = nColumns - match(TRUE, revRow=="1")+1 # find the new "first" match (even though it is the last match)
    } 
  }
  
  return(val)
}

# function to implement otis closure test
otisDetectClosure = function(df, nID = NULL){
  
  # store original dataset in case
  dfBackup = copy(df)
  
  # id is an optional argument to test specific simulations - used for checking validity of calculations
  if(!is.null(nID)){
    df = df %>%
      filter(id %in% nID)
  }
  
  
  # store distinct number of individuals captured for the combination number or simulation (simulation by id is optional)
  #nDistinctIndividuals = nrow(df) # too much work to add this
  
  # filter so that counts > 1 - or else the test doesn't really apply
  df = df%>%
    filter(counts>1)
  
  # handle case where *everyone* is captured only once so there is no data - highly unlikely, but here just in case
  # use original dataset to handle this, but make sure wi/vi/qi are all a weird number
  if(nrow(df)==0){
    df = dfBackup
    vi = NA
    wi = NA
    qi = NA
  } else {
    vi = apply(df,1,detectMin, bFirst=TRUE) # occasion of first capture
    wi = apply(df,1,detectMin, bFirst = FALSE) # wi is the occasion of last capture
  }
  
  df$vi = as.numeric(vi)
  df$wi = as.numeric(wi)
  df$qi= df$wi - df$vi # waiting time between first and last capture
  
  # get unique number of capture scenarios (not including single captures)
  lstK = unique(df$counts)
  lstCombinationNumber = unique(df$combinationNumber)
  
  # dataframe to store components of the test statistic
  dfComponents =  data.frame() # store estimates
  dfComponents = expand.grid(lstCombinationNumber, lstK) # get all possible combinations (not all will have data)
  colnames(dfComponents)= c("combinationNumber","k")
  nDfComponent = nrow(dfComponents) # get the number of combos for later use 
  
  for(nCombo in lstCombinationNumber){ # should I filter by combination number or simulation number??
    
    for (k in lstK){
      
      # subset data
      if(!is.null(nID)){
        dfK = df %>%
          filter(id %in% nID & combinationNumber == nCombo & counts == k)
      } else {
        dfK = df%>%
          filter(combinationNumber == nCombo & counts == k) 
      }
      
      # get time interval
      timeInterval = nchar(dfK[1,"ch"])
      fk = nrow(dfK) # number of individuals counted k times
      # get conditional expectation
      EQi = ((k-1)*(timeInterval+1))/(k+1)
      
      #print(dfK)
      
      # if no individuals exist that have been counted k times, the test statistic can't be calculated
      # for that scenario
      if(fk==0){
        EQ = NA
        VarQi = NA
        Ck= NA
      } else {
        # calculate test statistic
        EQ = sum(dfK$qi)/fk
        VarQi = (2*(timeInterval-k)*(k-1)*(timeInterval+1))/((k+2)*((k+1)^2)*fk )
        Ck = (EQ-EQi)/sqrt(VarQi) 
      }
      
      # add components to dataframe
      #dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"distinctIndividuals"] = nDistinctIndividuals
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"timeInterval"] = timeInterval
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"fk"] = fk
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"EQi"] = EQi
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"VarQi"] = VarQi
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"EQ"] = EQ
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"Ck"] = Ck
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"avgVi"] = mean(dfK$vi)
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"avgWi"] = mean(dfK$wi)
      dfComponents[dfComponents$combinationNumber == nCombo & dfComponents$k == k,"avgQi"] = mean(dfK$qi)
      
    }
    
  }
  
  # clean dfComponents so that only combinations with actual values are present
  dfComponents = dfComponents[dfComponents$fk !=0,]
  
  # add the overall test statistic C 
  dfComponents = dfComponents%>%
    group_by(combinationNumber)%>%
    mutate(C= sum(EQ - EQi)/sqrt(sum(VarQi)))
  
  return(dfComponents)
  
}