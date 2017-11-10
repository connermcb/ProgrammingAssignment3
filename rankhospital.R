## Function called `rankhospital` that takes three arguments: the 2-character 
## abbreviated name of a state (state), an outcome (outcome), and the ranking of
## a hospital in that state for that outcome (num). The function reads the 
## `outcome-of-care-measures.csv` file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

library(dplyr)
library(rlang)
library(stringr)

rankhospital <- function(state, outcome, num = "best", f="outcome-of-care-measures.csv") {
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
  
  ## Check that `outcome` is valid
  if(!(outcome %in% outcome_names)){
    stop(paste(outcome, "not in dataset"))
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcome_col <- parse_quosure(outcome)
  arrgnd_data <- outcome_data%>%
    select(outcome, Hospital.Name)%>%
    arrange(!!outcome_col, Hospital.Name)
  if(num=="best"){
    return(arrgnd_data[1])
  }
  if(num=="worst"){
    arrgnd_data <- arrgnd_data%>%
      na.omit()
    return(arrgnd_data[nrow(arrgnd_data),])
  }
  return(arrgnd_data$Hospital.Name[num])
  
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
