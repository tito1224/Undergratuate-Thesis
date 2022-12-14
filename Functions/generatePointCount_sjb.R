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

