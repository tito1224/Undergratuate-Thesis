generateDetects <- function(Ni,p_1m,maxMinute, seed = NULL){
  
  ## Initialize data frame with location, individual, and occasion
  pc_data <- tibble(locationID = 1:length(Ni)) %>%
    group_by(locationID) %>%
    summarize(individual = 1:Ni[locationID], .groups = "drop") %>%
    crossing(Minute = 1:maxMinute)
  
  ## Simulate detections
  pc_data <- pc_data %>%
    mutate(Detect = rbinom(n(), 1, p_1m))
  
  pc_data
}