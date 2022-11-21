# r script that contains functions to generate and analyze error free 
# point count data

# load libraries
library(tidyverse)
library(data.table)
library(matrixStats)

# predict the probability of detecting a bird at least once within m minutes
PredictProbability = function(m, p1m){
  # the way I think about this function is (for the time example): 
  # p1m is the probability of detecting an individual within a 1 min interval 
  # 1-p1m is the probability of not detecting an individual within a 1 min interval
  # (1-p1m)^m is the probability of not detecting an individual at all within m minutes
  # 1-((1-p1m)^m) is the probability of detecting an individual at least once within 
  # m minutes
  p_m = 1-((1-p1m)**m)
  return(p_m)
}


# generating point counts ci ~ Bin(Ni,p)
generatePointCount = function(Ni,p_1m,maxMinute){
  
  # if(!is.null(seed))
  #   set.seed(seed)
  
  if(Ni == 0){
    result <- list(NULL, 0)
  }
  else{
    individuals = 1:Ni
    survey_1 = as.data.frame(individuals)
    for(time in 1:maxMinute){
      # I'm saying each individual is either counted or not (bernoulli)
      # this data is already incremental :)
      
      sample_count = as.data.frame(rbinom(nrow(survey_1),1,p_1m))
      colnames(sample_count)=time
      survey_1 = cbind(survey_1,sample_count)
    }
    
    # do some manipulation to find total point count at the location 
    totalCount= sum(rowSums(survey_1[,as.character(1:maxMinute)]) > 0)
    result <- list(survey_1,totalCount)  
  }
  
  return(result)
}


# return point count data in incremental, cumulative, and summarized formats
returnData = function(Ni,p_1m,maxMinute, seed = NULL){
  dfPop = as.data.frame(N_i)
  
  if(!is.null(seed))
    set.seed(seed)
  result = sapply(N_i, generatePointCount,p_1m,maxMinute)
  dfPop$C_i = unlist(result[2,])
  
  # create unpack point count data and merge into one dataframe
  locationID = 1
  finalDF = matrix(NA,0,0)
  
  for (df in 1:length(result[1,])){
    if(!is.null(result[1,][[df]])){
      dfTemp = result[1,][[df]]
      dfTemp$locationID = locationID
      
      if(df==1){
        finalDF = dfTemp
      } else {
        finalDF = rbind(finalDF, dfTemp)
      }
    }
    
    locationID = locationID+1
  }
  
  # first number represents the location, second number represents the individual
  finalDF$UniqueIdentifier = paste(finalDF$locationID,finalDF$individuals)
  
  # find cumulative sum
  colsUse = as.character(1:maxMinute)
  tempdf = as.matrix(finalDF[,colsUse])
  tempdf = rowCumsums(tempdf)
  tempdf = as.data.frame(tempdf)
  colnames(tempdf) = colsUse
  
  # add some more details to the cumulative counts dataframe
  tempdf$locationID= finalDF$locationID
  tempdf$individuals = finalDF$individuals
  tempdf$UniqueIdentifier = finalDF$UniqueIdentifier
  tempdf$Detection = rowSums(tempdf[,1:maxMinute])
  
  
  # tempdf shows cumulative sum of counts for the individual
  # final df is the sparse matrix of detection history
  # dfPop is the summarised counts - generated from finalDF values
  return(list(tempdf, finalDF, dfPop))
  
}






