rankall_RJ <-
function(outcome, num = "best") { 
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
    
    ## DEBUG
    ##hospitals <- hospitals[hospitals$State != "GU",]
    ## END DEBUG
    
    ##return hostpital name for all states with lowest 30-day death rate
    for (col in outcome_columns) {
      hospitals[,col] <- as.numeric(hospitals[,col])
    }
    
    searchColumn <- paired_outcomes[paired_outcomes$valid_outcomes == outcome,"outcome_columns"]
    
    namesByState <- split(hospitals$Hospital.Name, hospitals$State)
    rankingsByState <- by(hospitals[,searchColumn], hospitals$State, order, na.last = NA, simplify = FALSE)
    
    if (num == "best") { 
      resultIdx <- sapply(rankingsByState, "[", 1)
    }
    else if (num == "worst") {
      resultIdx <- sapply(rankingsByState, length)
    }
    else {
      resultIdx <- sapply(rankingsByState, "[", num)
    }
    
    hospital <- sapply(seq_along(resultIdx), function(x) { return(namesByState[[x]][resultIdx[x]]) })
    state <- names(rankingsByState)
    
    return(data.frame(hospital, state))
}
