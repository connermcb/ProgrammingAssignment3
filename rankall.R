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

source("rankhospital.R")

library(dplyr)

rankall <- function(outcome, num = "best", f="outcome-of-care-measures.csv") {
  ## Read and subset outcome data
  if(!file.exists(f)){
    stop(paste("File:", f, "not found in wd"))
  }
  outcome_data <- read.csv(f)

  ## get list of state abbreviations
  st_abbrs <- sort(unique(outcome_data$State[!is.na(outcome_data$State)]))
  
  ## initialize dataframe for ranks
  ranks <- data.frame(matrix(nrow=0, ncol=2))
  colnames(ranks) <- c("hospital", "state")
  
  ## For each state, find the hospital of the given rank
  for(s in st_abbrs){
    nxt_st <- c(as.character(rankhospital(s, outcome, num)), s)
    dim(nxt_st) <- c(1, 2)
    colnames(nxt_st) <- c("hospital", "state")
    ranks <- rbind(ranks, nxt_st)
  }

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return(ranks)
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
