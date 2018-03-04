rankall <- function(outcome, num = "best") {
    outcomes <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character",
                         na.strings = "Not Available",
                         stringsAsFactors = FALSE)
    
    validOutcomes <- c("heart attack" = 11,
                       "heart failure" = 17,
                       "pneumonia" = 23)
    
    if(! outcome %in% names(validOutcomes)) {
        stop("invalid outcome")
    }

    states <- unique(outcomes[, "State"])
    
    # Filter by the relevant outcome
    outcomes <- data.frame("Hospital.Name" = outcomes[, 2],
                           "State" = outcomes[, 7],
                           "Outcome" =
                               as.numeric(outcomes[, validOutcomes[outcome]]),
                           stringsAsFactors = FALSE)
    
    # Remove NAs
    outcomes <- outcomes[!is.na(outcomes[,3]), ]
    
    df <- data.frame(hospital=character(),
                     state=character(),
                     stringsAsFactors = FALSE)
    
    for(state in states) {
        stateOut <- outcomes[outcomes[, "State"] == state, ]
        hospital <- NULL
        
        if(num == "best") {
            hospital <- stateOut[order(stateOut[,3],
                                   stateOut[,1]), ][1,1]
        } else if(num == "worst") {
            hospital <- stateOut[order(stateOut[,3],
                           stateOut[,1]), ][nrow(stateOut),1]
        } else {
            hospital <- stateOut[order(stateOut[,3],
                                   stateOut[,1]), ][num,1]
        }
        
        df[nrow(df) + 1, ] <- cbind(hospital, state)
    }
    
    df[order(df[,2]),]
}