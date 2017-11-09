## Function for Week 3 - R Programming Assignment
## Uses data from Hospital Compare web site (http://hospitalcompare.hhs.gov)
## run by the U.S. Department of Health and Human Services.


best <- function(state, outcome, f="outcome-of-care-measures.csv") {
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

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  min_rate <- min(as.numeric(as.matrix(outcome_data[, outcome])), na.rm = TRUE)
  lowest_idx <- which(as.numeric(as.matrix(outcome_data[, outcome]))==min_rate)
  return(as.vector(outcome_data$Hospital.Name)[lowest_idx])
}




