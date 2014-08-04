getmonitor <-
function(id, directory, summarize = FALSE) { 
  ## id = vector of length 1 indicating monitor ID number.
  ## user can specify 'id' as integer, character, or numeric
  
  ## directory = character vector of length 1 indicating location of CSV files
  
  ## summarize = logical indicating whether a summary should be output to the console
  
  id <- formatC(id, width=3, flag="0")
  filename <- sprintf("%s.csv",id)
  output <- read.csv(file = file.path(directory, filename), header = TRUE)
  if (summarize) {
    print(summary(output))
  }
  return(output)
}
