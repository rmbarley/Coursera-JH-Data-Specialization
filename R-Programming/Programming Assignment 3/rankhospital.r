rankhospital <- function(state, outcome, num = "best"){
    
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    data <- data[data$State == state, ]

    ranked <- rank(data[, column], na.last = NA)
    rankedCount <- length(ranked)
    
    ## Find and assign argument num to a real value
    if(num == "best"){
        i <- 1
    }
    else if(num <= rankedCount){
        i = num
    }
    else if(num == "worst"){
        i <- rankedCount
    }
    else if(num > rankedCount | num <= 0){
        return(NA)
    }
    else{
        stop('invalid num')
    }
    
    # Rank hospitals by num argument
    hospitalsRanked <- data$Hospital.Name[order(data[column], data$Hospital.Name)[i]]
    hospitalsRanked
}