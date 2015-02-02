## Title:

## Synopsis

## Data Processing
dir.create("./data", showWarnings=FALSE) 

if(!file.exists("./data/StormData.csv.bz2")){
    url = "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(url, destfile = "./data/StormData.csv.bz2", method = "auto")
}

con <- bzfile("./data/StormData.csv.bz2", "rt")
data <- read.csv(con, na.strings = "")
close(con)




dim(data)

## 

## levels(factor(data$PROPDMGEXP)
