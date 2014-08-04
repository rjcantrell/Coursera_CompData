agecount <- function(age = NULL) { 
  if (is.null(age) || is.na(as.numeric(age))) {
    stop("Invalid age!")
  }
  
  # fuck bitches, get data
  murder <- readLines("homicides.txt")
  
  searchCause = paste("", age, "[Yy]ears")
  return(length(grep(searchCause, murder)))
}
