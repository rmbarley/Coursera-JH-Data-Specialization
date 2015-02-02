## Write a function called best that take two arguments: the 2-character 
## abbreviated name of a state and an outcome name. The function reads the 
## outcome-of-care-measures.csv file and returns a character vector with the 
## name of the hospital that has the best (i.e. lowest) 30-day mortality for the 
## specified outcome in that state. The hospital name is the name provided in 
## the Hospital.Name variable. The outcomes can be one of 
## "heart attack", "heart failure", or "pneumonia". Hospitals that do not have 
## data on a particular outcome should be excluded from the set of hospitals when 
## deciding the rankings.

## 1 Plot the 30-day mortality rates for heart attack
## outcome <- read.csv("./data/outcome-of-care-measures.csv", 
   ##                 colClasses ="character")
## outcome[, 11] <- as.numeric(outcome[, 11])
## hist(outcome[, 11])

## 2 Finding the best hospital in a state

best <- function(state, outcome){
    
    ##Read outcome data
    data <- read.csv("./data/outcome-of-care-measures.csv", 
                     colClasses ="character")
    
    ## Check that state and outcome are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!state %in% data$State){
        stop('invalid state')
    }
    else if(!outcome %in% outcomes){
        stop('invalid outcome')
    }
    else{
        if(outcome == "heart attack"){
            ## Column 11
            column <- 11
        }
        else if(outcome == "heart failure"){
            ## Column 17
            column <- 17
        }
        else if(outcome == "pneumonia"){
            ## Column 23
            column <- 23  
         }
    }
    ## Convert character columns to numeric
    data[, column] <- as.numeric(data[, column])
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    df <- data[data$State == state, c(2, column)]
    df <- df[which.min(df[, 2]), 1]
    na.omit(df)
}