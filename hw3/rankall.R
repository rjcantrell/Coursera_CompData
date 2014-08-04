rankall <- function(outcome, num = "best") { 
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_columns <- c(11, 17, 23)
  paired_outcomes <- data.frame(valid_outcomes, outcome_columns)
  searchColumn <- paired_outcomes[paired_outcomes$valid_outcomes == outcome,"outcome_columns"]
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## DEBUG
  ## outcome <- outcome[outcome$State != "GU",]
  ## END DEBUG
  
  hospital <- outcome$Hospital.Name
  state <- outcome$State
  search <- outcome[,searchColumn]
  outcome_sub <- data.frame(hospital, state, search)
  
  hosp.rank.all<-do.call(rbind,lapply(sort(unique(outcome_sub$state)), function(x,hosp.index) {
    
    state.orig.df<-outcome_sub[outcome_sub$state==x,]
    state.df<-state.orig.df[complete.cases(state.orig.df),]
    state.df<-state.df[order(state.df[,3],state.df$hospital),]
    state.df<-state.df[rank(state.df[,3],state.df$hospital,ties.method="first"),]
    state.df$hosp.rank<-seq(1:nrow(state.df))
    
    if( hosp.index=="best") hosp.index <-1
    else if (hosp.index=="worst") hosp.index <-nrow(state.df)
    #else hosp.index<-hosp.index
    
    state.rank.df<-merge(data.frame(state=x),state.df[hosp.index,],by="state",all.x=TRUE)
    rownames(state.rank.df)<-x
    
    return(state.rank.df[,c("hospital","state")])
    
  },hosp.index=num ))
  
  return(hosp.rank.all)
}