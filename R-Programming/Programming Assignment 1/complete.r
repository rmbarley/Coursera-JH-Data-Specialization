complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  #initialize ids and nobs to empty vectors
  ids <- vector()
  nobs <- vector()
  
  files_list <- list.files("specdata", full.names = T)
  dat <- data.frame()
  for(i in id){
    dat <- rbind(dat, read.csv(files_list[i]))
    ## Store the id
    ids <- c(ids, i)
    
    ## Find the complete cases
    good <- dat[complete.cases(dat),]
    nobs <- c(nobs, nrow(good))
  }
  
  data.frame(id=ids, nobs=nobs)  
}