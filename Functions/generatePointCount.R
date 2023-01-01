# Implement functions that Dr. Bonner suggested
# only change is that I merged the generateMovement and splitDetects function into one function

library(tidyverse)
library(data.table)
library(matrixStats)


# Create a function to model detection probability by time m
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

# generate "regular" data
# incremental detections
generateDetects <- function(Ni,p_1m,maxMinute, seed = NULL){
  
  ## Set seed
  if (!is.null(seed))
    set.seed(seed)
  
  ## Initialize data frame with location, individual, and occasion
  ## Locations with no individuals are removed
  pcData <- tibble(locationID = 1:length(Ni),
                   N = Ni) %>%
    filter(N > 0) %>%
    group_by(locationID) %>%
    summarize(individual = 1:N, .groups = "drop") %>%
    crossing(Minute = 1:maxMinute)
  
  ## Simulate detections
  pcData <- pcData %>%
    mutate(Detect = rbinom(n(), 1, p_1m))%>%
    group_by(locationID,individual)%>%
    mutate(ch=paste(Detect,collapse="")) %>% # add count history column
    ungroup()
  
  pcData
}

# helper function to transform data to wide format
# incremental detections
detectsToCapHist <- function(pcData, keepZeros = FALSE){
  
  ## Remove individuals that were never detected
  if(!keepZeros){
    
    ## Compute number of detections per individual
    pcData <- pcData %>%
      group_by(locationID,individual) %>%
      mutate(Detects = sum(Detect)) %>%
      filter(Detects > 0) %>%
      select(-Detects)
  }
  
  ## Pivot data
  chData <- pcData %>%
    mutate(Minute = paste0("Minute",Minute)) %>%
    pivot_wider(names_from = Minute,
                values_from = Detect)
  
  chData
}

generateMovement <- function(pcData, alpha, seed = NULL){
  
  ## Set seed
  if (!is.null(seed))
    set.seed(seed)
  
  ## Simulate movements
  pcData <- pcData %>%
    mutate(Move = (Minute > 1) * rbinom(n(), 1, alpha))
  
  
}

splitDetects <- function(pcData){
  ## Identify split histories for each individual
  pcData <- pcData %>%
    group_by(locationID, individual) %>%
    mutate(Split = cumsum(Move) + 1) %>%
    ungroup()
  
  ## Split histories and fill remaining values with 0s
  pcData <- pcData %>%
    mutate(individual = paste0(individual,"_",Split)) %>%
    select(-Move, -Split) %>%
    complete(nesting(locationID, individual), Minute, fill = list(Detect = 0))
  
  pcData
}

# merge generateMovement and splitDetects functions
# I'll use this function in the .Rmd but keep the splitDetects() and generateMovement()
# for debugging purposes 
generateErrors = function(pcData,alpha, seed = NULL){
  
  # add movement
  dfMove = generateMovement(pcData, alpha, seed)
  
  # add new individuals
  dfSplit = splitDetects(dfMove)
  
  # update ch column 
  
  dfSplit = dfSplit %>%
    group_by(locationID,individual)%>%
    mutate(ch=paste(Detect,collapse="")) %>% # add count history column
    ungroup()
  
  return(dfSplit)
  
}

# format data to be summarised, or cumulative
# input data in long format
formatData = function(pcData, isSummarised= FALSE, isCumulative =FALSE){
  formats = c(isSummarised, isCumulative)
  colsUse = paste0("Minute",1:maxMinute)
  
  if (length(formats[formats==TRUE])>1){
    return("please choose one format")
  }
  
  if(isSummarised == TRUE){
    dfSummary =  detectsToCapHist(pcData,keepZeros = TRUE)
    
    # do some manipulation to find total point count at the location 
    # doing it this way because I don't want my results to be affected by 
    # whether the .keepZeros parameter is used or not in the detectToCapHist() function
    dfSummary$TotalCount = ifelse(rowSums(dfSummary[,colsUse])>=1,1,0)
    dfSummary = dfSummary %>%
      group_by(locationID)%>%
      summarise(C_i = sum(TotalCount))
    
    return(dfSummary)
    
  } else if(isCumulative==TRUE){
    dfCumulative = detectsToCapHist(pcData,keepZeros = TRUE)
    
    # find cumulative counts
    dftemp = as.matrix(dfCumulative[,colsUse])
    dftemp = rowCumsums(dftemp)
    dftemp = as.data.frame(dftemp)
    colnames(dftemp) = colsUse
    
    # join back to original dataframe
    dfCumulative = select(dfCumulative, !c(colsUse))
    dfCumulative = cbind(dfCumulative, dftemp)
    return(dfCumulative)
  }
  
  
}
