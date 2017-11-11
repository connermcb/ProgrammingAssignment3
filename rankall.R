## `rankall` takes two arguments: an outcome name (`outcome`) and a hospital ranking
## (`num`). The function reads the outcome-of-care-measures.csv file and returns
## a 2-column data frame containing the hospital in each state that has the 
## ranking specified in num. For example the function call 
## rankall("heart attack", "best") would return a data frame containing the 
## names of the hospitals that are the best in their respective states for 
## 30-day heart attack death rates. The function should return a value for every
## state (some may be NA). The first column in the data frame is named hospital,
## which contains the hospital name, and the second column is named state, which
## contains the 2-character abbreviation for the state name. Hospitals that do
## not have data on a particular outcome should be excluded from the set of 
## hospitals when deciding the rankings.

rankall <- function(outcome, num = "best", f="outcome-of-care-measures.csv") {
  ## Format args
  state <- toupper(state)
  outcome <- str_replace_all(tolower(outcome), pattern = " ", repl="")
  
  ## Read and subset outcome data
  if(!file.exists(f)){
    stop(paste("File:", f, "not found in wd"))
  }
  outcome_data <- read.csv(f)
  outcome_data <- subset(outcome_data, State==state)
  
  ## Rename columns of interest
  outcome_names <- c("heartattack", "heartfailure", "pneumonia")
  colnames(outcome_data)[c(11, 17, 23)] <- outcome_names 
  
  ## Change class of variable
  outcome_data[, outcome] <- as.numeric(
    as.matrix(
      outcome_data[, outcome]
    )
  )
  ## Check that `state` is valid
  state_names <- unique(outcome_data$State)
  if(!(state %in% state_names)){
    stop(paste(state, "not found in dataset"))
  }
  
  ## Check that value of `outcome` is valid
  if(!(outcome %in% outcome_names)){
    stop(paste(outcome, "not in dataset"))
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
