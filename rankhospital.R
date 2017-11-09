## Function called `rankhospital` that takes three arguments: the 2-character 
## abbreviated name of a state (state), an outcome (outcome), and the ranking of
## a hospital in that state for that outcome (num). The function reads the 
## `outcome-of-care-measures.csv` file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
  ## Format args
  state <- toupper(state)
  outcome <- tolower(outcome)
  
  ## Read and subset outcome data
  if(!file.exists(f)){
    stop(paste("File:", f, "not found in wd"))
  }
  outcome_data <- read.csv(f)
  outcome_data <- subset(outcome_data, State==state)
  
  ## Rename columns of interest
  outcome_names <- c("heart attack", "heart failure", "pneumonia")
  colnames(outcome_data)[c(11, 17, 23)] <- outcome_names 
  
  ## Check that `state` is valid
  state_names <- unique(outcome_data$State)
  if(!(state %in% state_names)){
    stop(paste(state, "not found in dataset"))
  }
  
  ## Check that `outcome` is valid
  if(!(outcome %in% outcome_names)){
    stop(paste(outcome, "not in dataset"))
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
