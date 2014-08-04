count <- function(cause = NULL) { 
  # validate input
  valid_causes = c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  if (is.null(cause) || !(cause %in% valid_causes)) {
    stop("Invalid cause of death specified!")
  }
  
  # fuck bitches, get data
  murder <- readLines("homicides.txt")
  
  # setup search criteria
  cause_First <- substring(cause, 1, 1)
  cause_Rest <- substring(cause, 2, nchar(cause))
  searchCause <- paste("[cC]ause:", paste("[", cause_First, toupper(cause_First), "]", cause_Rest, sep = ""))  
  
  return(length(grep(searchCause, murder)))
}
