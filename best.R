## Function for Week 3 - R Programming Assignment
## Uses data from Hospital Compare web site (http://hospitalcompare.hhs.gov)
## run by the U.S. Department of Health and Human Services.


best <- function(state, outcome, f="outcome-of-care-measures.csv") {
  ## Format args
  state <- toupper(state)
  outcome <- tolower(outcome)
  
  ## Read outcome data
  if(!file.exists(f)){
    stop(paste("File:", f, "not found in wd"))
  }
  outcome_data <- read.csv(f)
  
  ## Check that `state` is valid
  state_names <- unique(outcome_data$State)
  if(!(state %in% state_names)){
    stop(paste(state, "not found in dataset"))
  }
  ## Check that `outcome` is valid
  process_colnames <- function(cn){
    cn_split <- strsplit(cn, split=".", fixed=TRUE)[[1]]
    return(tolower(cn_split[length(cn_split)]))
  }
  
  outcome_names <- lapply(colnames(outcome_data), process_colnames)
  outcome_names <- unique(outcome_names)
  
  if(!(outcome %in% outcome_names)){
    stop(paste(outcome, "not in dataset"))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  hsptl_names <- unique()
}


