## Function called `rankhospital` that takes three arguments: the 2-character 
## abbreviated name of a state (state), an outcome (outcome), and the ranking of
## a hospital in that state for that outcome (num). The function reads the 
## `outcome-of-care-measures.csv` file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
