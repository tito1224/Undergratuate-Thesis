# functions needed to generate errors in point count data

# load libraries
library(tidyverse)
library(data.table)
library(matrixStats)
source("./Functions/generatePointCount.R")

# create a latent matrix of movements based on incremental point count data
# doing it based on incremental point count data so that I don't need to worry 
# about locations where the site is 0
generateLatentMatrix = function(dfPointCount,alpha,maxMinute, seed = NULL){
  if(!is.null(seed))
    set.seed(seed)
  
  # get number of individuals at each location 
  dfSummary = dfPointCount %>%
    group_by(locationID)%>%
    summarise(count = n(),
              locationID = max(locationID))
  N = dfSummary$count
  
  finalDF = matrix(NA,0,0)
  for(row in 1:length(N)){
    
    # grab site specific count and store locationID & unique identifier
    Ni = N[row]
    locationID = dfSummary[row,]$locationID
    
    # begin creating dataframe to store movement
    individuals = 1:Ni
    latent_1 = as.data.frame(individuals)
    
    for(time in 1:(maxMinute-1)){
      
      # the time variable in this case will represent what happens *after* that time number
      # so data in column "1" represents if the bird moved *after* time 1
      
      # I'm saying each individual moves or not (bernoulli) w.p. alpha
      sample_movement = as.data.frame(rbinom(nrow(latent_1),1,alpha))
      colnames(sample_movement)=time
      latent_1 = cbind(latent_1,sample_movement)
      
    }
    result = latent_1
    result$locationID = locationID
    result$UniqueIdentifier = paste(result$locationID,result$individuals)
    
    # add to finalDF
    if(nrow(finalDF)==0){
      finalDF = result
    } else {
      finalDF = rbind(finalDF,result)
    }
  }
  
  return(finalDF)
  
}

# generate erroneous point count data using latentMatrix
generateErrors = function(dfPointCount,maxMinute,alpha, seed = NULL){
  
  latentMatrix = generateLatentMatrix(dfPointCount,alpha,maxMinute,seed)
  
  # handle the cases where latentMatrix is NULL (because population is zero at that location)
  # actually not needed anymore since I'm using the granular point count data to 
  # generate errors - but will keep for now. clean up later 
  if(is.null(latentMatrix)){
    finalDF = NULL
  } else {
    finalDF = matrix(NA,0,0)
    for(row in 1:nrow(latentMatrix)){
      
      # select a row to isolate detection history and movement history for an individual
      locationID = dfPointCount[row,]$locationID
      originalHistory = as.numeric(dfPointCount[row,2:(maxMinute+1)]) # this will be updated as we loop through time intervals and make splits
      latentVector = as.numeric(latentMatrix[row,2:(maxMinute)])
      
      # figure out which columns have movement
      timeMovement = which(latentVector==1)
      counter = 0 # to count how many splits have happened so far
      
      # handle case where no movement occurs
      if(length(timeMovement)==0){
        tempDF = as.data.frame(originalHistory)
        tempDF = t(tempDF)
        rownames(tempDF) = NULL
        colnames(tempDF) = 1:maxMinute
        tempDF = as.data.frame(tempDF)
        tempDF$individuals = row
        tempDF$individuals = as.character(tempDF$individuals)
        tempDF$locationID = locationID
        
        # return two vectors!
        if(nrow(finalDF)==0){
          finalDF = tempDF
        } else {
          finalDF = bind_rows(finalDF, tempDF)
        }
        
      } else {
        # loop through the time intervals with movement and perform splits at each 
        # time interval
        for(time in timeMovement){
          
          # handle case where the bird is detected only once
          # doing this because if bird is detected only one time, we can't have an
          # error where they are double counted -> the only error would a labeling
          # error and we aren't looking to quantify that right now
          if(sum(originalHistory[-c(1:time)])<=1){
            tempDF = as.data.frame(originalHistory)
            tempDF = t(tempDF)
            rownames(tempDF) = NULL
            colnames(tempDF) = 1:maxMinute
            tempDF = as.data.frame(tempDF)
            # lack of a letter denotes that a split was indiciated by the latent 
            # matrix, but would have been a labelling error instead of a double
            # counting error
            tempDF$individuals = paste0(row,"_",time) 
            tempDF$locationID = locationID
            
            # return two vectors!
            if(nrow(finalDF)==0){
              finalDF = tempDF
            } else {
              finalDF = bind_rows(finalDF, tempDF)
            }
          } else {
            # keep old vector values from before movement
            new_time = numeric(maxMinute)
            new_time[1:time] = originalHistory[1:time]
            
            # add new individual and give them remaining counts
            new_time_move = numeric(maxMinute)
            new_time_move[-c(1:time)] = originalHistory[-c(1:time)]
            
            # bind the two vectors together
            tempDF = rbind(new_time, new_time_move)
            colnames(tempDF) = 1:maxMinute
            rownames(tempDF) = NULL
            tempDF = as.data.frame(tempDF)
            tempDF$individuals = paste0(row,"_",time,letters[1:2])
            tempDF$locationID = locationID
            
            # update originalHistory vector so that it is the vector of the "new" individual
            originalHistory = new_time_move
            counter = counter + 1 # update counter 
            
            # add case to remove redundant rows
            if(counter<length(timeMovement)){
              tempDF = tempDF %>%
                filter(individuals != paste0(row,"_",time,"b"))
            }
            
            # return two vectors!
            if(nrow(finalDF)==0){
              finalDF = tempDF
            } else {
              finalDF = bind_rows(finalDF, tempDF)
            }  
          }
          
        } 
      }
    }
  }
  
  # remove duplicated rows 
  return(finalDF)
}


# return cumulative, incremental and summarized erroroneous data
returnErrors = function(N_i, p_1m, maxMinute, alpha,seed = NULL){
  # N_i is a vector
  
  # generate point count data
  result = returnData(N_i,p_1m,maxMinute, seed = seed)
  dfPointCount = result[[2]]
  dfPointCount_Summarized = result[[3]]
  dfPointCount_Summarized$locationID = 1:nrow(dfPointCount_Summarized)
  
  # generate errors
  dfError = generateErrors(dfPointCount, maxMinute, alpha,seed)
  
  # generate summarized data
  dfError$Detection = ifelse(rowSums(dfError[,1:maxMinute])>0,1,0)
  dfError_Summarized =  dfError %>%
    group_by(locationID) %>%
    summarise(C_i_error = sum(Detection))
  dfError_Summarized = left_join(dfPointCount_Summarized, dfError_Summarized, by= "locationID")
  
  # NAs appear because dfErrors dataframe was generated using point data -> and that 
  # is only available for individuals that have been detected *at least once*
  # so I'm just going to do a fill of NA with 0
  dfError_Summarized[is.na(dfError_Summarized$C_i_error),"C_i_error"]=0
  
  # generate cumulative data
  colsUse = as.character(1:maxMinute)
  tempdf = as.matrix(dfError[,colsUse])
  tempdf = rowCumsums(tempdf)
  tempdf = as.data.frame(tempdf)
  colnames(tempdf) = colsUse
  
  ## add some more details to the cumulative counts dataframe
  tempdf$locationID= dfError$locationID
  tempdf$individuals = dfError$individuals
  
  return(list(tempdf, dfError, dfError_Summarized))
}


