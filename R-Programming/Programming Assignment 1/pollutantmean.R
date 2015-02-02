pollutantmean <- function(directory, pollutant, id = 1:332) {
  ##https://github.com/edj-boston/coursera-r-programming/blob/master/programming-assignment-1/complete.R
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  #Download the files, if they are not already in the workspace
  if(!file.exists("specdata.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
    download.file(url, destfile = "specdata.zip")
    unzip("specdata.zip")
  }
  
  #Create a data frame containing all of the entries
  files_list <- list.files("specdata", full.names = T)
  dat <- data.frame()
  for(i in id){
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  
  #Create a subset with the pollutant we want to study
  dat_subset <- dat[, pollutant]
  
  #Find and print the mean
  dat_mean <- mean(dat_subset, na.rm = T)
  round(dat_mean, 3)
  
}
