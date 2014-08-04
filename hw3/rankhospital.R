rankhospital <- function(state, outcome, num = "best") { 
  ##fucking options, yo
  options(warn=-1)
  
  ##setup variables we need
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_columns <- c(11, 17, 23)
  paired_outcomes <- data.frame(valid_outcomes, outcome_columns)
  
  ##check that outcome is not an invalid entry
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  ##read outcome data from file (outcome-of-care-measures.csv)
  hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##check that state exists in file data
  if (!(state %in% hospitals$State)) {
    stop("invalid state")
  }
  
  ##return hostpital name in that state with lowest 30-day death rate
  for (col in outcome_columns) {
    hospitals[,col] <- as.numeric(hospitals[,col])
  }
  
  ourState <- hospitals[hospitals$State == state,]
  searchColumn <- paired_outcomes[paired_outcomes$valid_outcomes == outcome,"outcome_columns"]
  
  #THIS IS LITERALLY THE ONLY LINE THAT MUST CHANGE, WHAAAAAAAAAT
  rankings <- order(ourState[,searchColumn], ourState$Hospital.Name, na.last = NA)
  
  if (num == "best") { 
    resultRow <- rankings[1]
  }
  else if (num == "worst") {
    resultRow <- rankings[length(rankings)]
  }
  else {
    resultRow <- rankings[num]
  }
  
  return(ourState[resultRow,]$Hospital.Name)
}