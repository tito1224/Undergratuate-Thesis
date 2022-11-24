generateDetects <- function(Ni,p_1m,maxMinute, seed = NULL){
  
  ## Initialize data frame with location, individual, and occasion
  pcData <- tibble(locationID = 1:length(Ni)) %>%
    group_by(locationID) %>%
    summarize(individual = 1:Ni[locationID], .groups = "drop") %>%
    crossing(Minute = 1:maxMinute)
  
  ## Simulate detections
  pcData <- pcData %>%
    mutate(Detect = rbinom(n(), 1, p_1m))
  
  pcData
}

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

generateMovement <- function(pcData, alpha){
  
  ## Simulate movements
  pcData <- pcData %>%
    mutate(Move = (Minute > 1) * rbinom(n(), 1, alpha))
  
  pcData
}

