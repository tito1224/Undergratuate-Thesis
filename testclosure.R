# set MarkPath (only for running on supercomputer)
#MarkPath = "/home/oadebajo/bin"

# load libraries
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


######## LOAD DATA ########

# will need up update the directory used here when I run on sharcnet
loadData = function(strPath = "./DigitalAllianceOutput/Huggins"){
  setwd(strPath)
  dataFinalTrial = list.files(pattern = ".rds") # load rds files
  dataFinalTrial = mixedsort(dataFinalTrial)%>% # use mixedsort so it appears in ascending order 
    map(readRDS)
  
  # initialize empty dataframes to store the data
  dfHistoryFinalH = data.frame()
  #counter = 1
  
  # loop through the rds files and retrive data 
  for (item in dataFinalTrial){
    tempHistory= item[[3]] # load the detection histories used to generate the histories 
    dfHistoryFinalH = rbind(dfHistoryFinalH, tempHistory)
    #counter = counter + 1
  }
  
  # id is combinationNumber_simulationNumber
  dfHistoryFinalH$id = paste0(dfHistoryFinalH$combinationNumber,"_",dfHistoryFinalH$simulationNumber) 
  
  # this code is what was used in the ./Functions/runSimulation.R file to generate scenarios to input into the HPC 
  nRuns = 1000
  lstNi = c(10,20)
  lstP = c(0.1,0.2,0.3,0.4,0.5)
  #lstAlpha = c(0)
  lstAlpha =c(0,0.05,0.1,0.2,0.3)
  lstMaxMin = c(5,10)
  lstFormula = c("~1")
  lstMixtures = c(1)
  lstSeed = c("NULL")
  strModel = "Huggins"
  
  params = expand.grid(nRuns,lstNi,lstP, lstAlpha, lstMaxMin,lstFormula,lstMixtures,lstSeed,strModel)
  colnames(params) = c("nRuns","N","p_1m","alpha","maxMinute","strFormula","lstMixtures","lstSeed","strModel")
  params$Scenario = 1:nrow(params)
  
  # merge the params dataframe with the dataframe of estimates to help with calculating summary statistics 
  dfHistoryFinalH = left_join(dfHistoryFinalH,params,by=c("combinationNumber"="Scenario")) 
  
  colnamesUse= paste0("Minute",1:10) # should say 1:10
  dfHistoryFinalH$counts = rowSums(dfHistoryFinalH[,colnamesUse],na.rm=TRUE)
  dfHistoryFinalH$bCounts = ifelse(dfHistoryFinalH$counts > 1,">1","=1")
  
  # dfHistoryFinalH=dfHistoryFinalH%>%
  #   filter(id %in% c("80_100","80_150"))
  
  return(dfHistoryFinalH)
}

######### FIT MODELS #################

testGOF_CJS = function(ch,str_pFormula="~1",str_phiFormula="~1",strModel="CJS",scenarioSimNum="40"){
  # fixed param can either be "p" or "phi"
  print("starting fit model")
  
  # set up formulas for the model
  pformula_unconstrained = list(formula = eval(parse_expr(str_pFormula)),share=TRUE)
  phiformula_unconstrained = list(formula = eval(parse_expr(str_phiFormula)))
  
  # add restrictions if necessary
  phiformula_constrained = list(formula = eval(parse_expr(str_phiFormula)), fixed = 1) # everyone survives (so a closed population)
  
  # fit the models
  # this is a test the unconstrained CJS model vs a closed population model (where we constrain phi = 1)
  scenarioSimNum_unconstrained = paste0(scenarioSimNum,"_u")
  scenarioSimNum_constrained = paste0(scenarioSimNum,"_c")
  modelUnconstrained = mark(ch, model = strModel, model.parameters = list(Phi=phiformula_unconstrained,p=pformula_unconstrained),prefix=scenarioSimNum_unconstrained,delete=TRUE,output=FALSE,model.name = scenarioSimNum_unconstrained ) 
  modelConstrained = mark(ch, model = strModel, model.parameters = list(Phi=phiformula_constrained,p=pformula_unconstrained),prefix=scenarioSimNum_constrained,delete=TRUE,output=FALSE, model.name= scenarioSimNum_constrained) 
  
  # extract estimates and confidence intervals
  # bConstrained is a boolean column to identify if a value is constrained or not
  dfOutputUnconstrained = modelUnconstrained$results$real%>%
    mutate(bConstrained= 0,
           id=scenarioSimNum)
  dfOutputConstrained = modelConstrained$results$real %>%
    mutate(bConstrained = 1,
           id=scenarioSimNum)
 
  dfOutput = rbind(dfOutputUnconstrained, dfOutputConstrained)
  dfOutput$param = rownames(dfOutput)
  # rename estimate column 
  dfOutput = dfOutput %>%
    rename(CJS_estimate = estimate)
  
  
  ## Goodness of fit testing ##
  #H0: simpler (nested/constrained) model is true
  #HA: more general/unconstrained model is true
  
  # grab chat values using TEST 2 and TEST 3 and decide based on chat whether to use quasi likelihood
  #df_processed = process.data(ch,model=strModel)
  #dfChiSquare = release.gof(df_processed,title = scenarioSimNum)
  #c_hat = dfChiSquare[3,"Chi.square"]/dfChiSquare[3,"df"]
  
  # if c_hat < 1 do nothing, else, use QAICc and quasi likelihood
  all_cjs_models = collect.models(type = "CJS")
  #if (c_hat <1){
  #  all_cjs_models$model.table$p_CJS = pchisq(all_cjs_models$model.table[2,"Deviance"] - all_cjs_models$model.table[1,"Deviance"],1,lower.tail=FALSE)
  #} else {
  #  all_cjs_models =adjust.chat(c_hat_test,all_cjs_models)
  #  all_cjs_models$model.table$p_CJS = pchisq(all_cjs_models$model.table[2,"QDeviance"] - all_cjs_models$model.table[1,"QDeviance"],1,lower.tail=FALSE)
  #}
  
  all_cjs_models$model.table$p_CJS = pchisq(all_cjs_models$model.table[2,"Deviance"] - all_cjs_models$model.table[1,"Deviance"],1,lower.tail=FALSE)
  all_cjs_models$model.table$id = scenarioSimNum
  all_cjs_models$model.table$bConstrained = ifelse(all_cjs_models$model.table$npar>1,0,1)
  
  dfOutput = left_join(dfOutput, all_cjs_models$model.table, by = c("id","bConstrained"))
  return(dfOutput)
}


# function to test GOF
testGOF_RELEASE = function(df){
  ch = df[,"ch"]
  
  # if all zero's are encountered, skip
  if(sum(df$counts)==0){
    chat = NA
  } else {
    ch_processed =process.data(ch,model="CJS")
    
    # sometimes an error occurs where it the program terminates and idk what causes it
    # an example is if we use id 1_1
    # in that case I will write a tryCatch and set the chat value to NA
    tryCatch({
      ch_RELEASE = release.gof(ch_processed)
    },error=function(e){} )
    
    if(!exists("ch_RELEASE")){
      chat = NA
    } else{
      chat = ch_RELEASE[3,"Chi.square"]/ch_RELEASE[3,"df"] 
    }
    
  }
  dfChat = tibble(chat_val = chat)
  return(dfChat) 
}


####### RUN CLOSURE TESTS #####

runAllClosureTests = function(nCombinationNumber= 1,strPath = "./FinalOutput" ){
  dfAllData= loadData(strPath = strPath) # update strPath for running on HPC
  dfAllData = dfAllData%>%
    filter(combinationNumber == nCombinationNumber)%>%
    filter(id %in% c("1_1","1_2","1_3"))
  lstUniqueID = unique(dfAllData$id)
  
  dfCJS = data.frame()
  
  # run CJS tests
  for (tempID in lstUniqueID){
    dfTemp = dfAllData %>%
      filter(id == tempID)
    
    #print("ch being used")
    #print(dfTemp[,c("id","individual","ch")])
    
    # do not use cases where no individual was detected
    if(sum(dfTemp$counts)==0){
      next()
    }  
    
    dfTempCJS = testGOF_CJS(ch = dfTemp[,"ch"], scenarioSimNum = tempID)
    #print("CJS results")
    #print(dfTempCJS)
    dfCJS = rbind(dfCJS, dfTempCJS)
    
  }
  
  #print("final df before joining")
  #print(dfAllData[,c("id","individual","ch")])
  #print(dfCJS)
  #df_CJS_Analysis = left_join(dfAllData, dfCJS, by = "id")
  #print(dfAllData[,c("id","individual","ch")])
  
  # run OTIS Tests
  #df_OTIS_Analysis = otisDetectClosure(dfAllData)[,c("id","vi","wi","qi","fk","EQi","EQ","VarQi","Ck","p_k_Otis","C","pOverall_Otis","avgQi")]
  #dfFinalData = left_join(df_CJS_Analysis, df_OTIS_Analysis, by = "id")
  #print("final result")
  #print(dfFinalData)
  
  return(dfCJS)
}


# return results (for HPC)
# set array to 1-100
# uncomment the bottom to run on hpc

## Read command line arguments
#args = commandArgs(trailingOnly=TRUE)

## Set job number
#id = as.integer(args[1]) 
#id=1
# set up parameters
#nRuns = 1000
#lstNi = c(10,20)
#lstP = c(0.1,0.2,0.3,0.4,0.5)
#lstAlpha = c(0)
#lstAlpha =c(0,0.05,0.1,0.2,0.3)
#lstMaxMin = c(5,10)
#lstFormula = c("~1")
#lstMixtures = c(1)
#lstSeed = c("NULL")
#strModel = "Huggins"

#params = expand.grid(nRuns,lstNi,lstP, lstAlpha, lstMaxMin,lstFormula,lstMixtures,lstSeed,strModel)
#colnames(params) = c("nRuns","lstNi","lstP","lstAlpha","lstMaxMin","lstFormula","lstMixtures","lstSeed","strModel")
#params$lstFormula = as.character(params$lstFormula) # for some reason this column turns into a factor variable?
#params$lstSeed = as.character(params$lstSeed) # for some reason i need to wrap this with as.character()
#params$Scenario = 1:nrow(params)


#results = runAllClosureTests(nCombinationNumber=params[id,"Scenario"], strPath = "./FinalOutput")
#outfile = paste0("Closure2/closure_test_CJS",id,".rds") # note that the Closure folder is in the FinalOutput folder
#saveRDS(results,outfile)


