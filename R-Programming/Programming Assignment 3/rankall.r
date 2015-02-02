rankall <- function(outcome, num = "best"){
    
    ##Read outcome data
    data <- read.csv("./data/outcome-of-care-measures.csv", 
                     colClasses ="character")
    
    ## Subset and sort state data, getting only one instance of each state
    state <- sort(unique(data$State))
    hospitalArray <- rep("", length(state))  
    
    ## Check that outcomes are valid
    for(i in 1:length(state)){
        stateData <- data[data$State == state[i],]
        
        if(outcome == "heart attack"){
            ## Column 11
            outcomes <- as.numeric(stateData[, 11])        
        }
        else if(outcome == "heart failure"){
            ## Column 17
            outcomes <- as.numeric(stateData[, 17])        
        }
        else if(outcome == "pneumonia"){
            ## Column 23
            outcomes <- as.numeric(stateData[, 23])        
        }
        else{
            stop('invalid outcome')
        }
        
        rankedOutcomes <- rank(outcomes, na.last=NA)
        rankedCount <- length(rankedOutcomes)
        
        ## Find and assign argument num to a real value
        if(num == "best"){
            j <- 1
        }
        else if(num <= rankedCount){
            j = num
        }
        else if(num == "worst"){
            j <- rankedCount
        }
        else if(num > rankedCount | num <= 0){
            j <- NA
        }
        else{
            stop('invalid num')
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        if(is.na(j)){
            hospitalArray[i] <- NA
        }
        else{
            hospitalArray[i] <- stateData$Hospital.Name[order(outcomes, 
                                stateData$Hospital.Name)[j]]
        }
    }
    
    rankeddf <- data.frame(hospital = hospitalArray, state = state)
    rankeddf
}