complete <-
function(directory, id = 1:332) { 
  # directory = character vector of length 1 indicating location of CSV files
  
  # id = integer vector indicating monitor ID numbers to be used
  output <- data.frame(id = id, nobs = id)
  
  for(i in id)  {
    data <- getmonitor(i, directory)
    output$nobs[output$id == i] <- sum(complete.cases(data))
  }
  
  return(output)
}