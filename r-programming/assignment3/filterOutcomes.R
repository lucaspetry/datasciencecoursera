filteredOutcomes <- function(state, outcomeName) {
    outcomes <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character",
                         na.strings = "Not Available",
                         stringsAsFactors = FALSE)
    
    validStates <- unique(outcomes[,"State"])
    validOutcomes <- c("heart attack" = 11,
                       "heart failure" = 17,
                       "pneumonia" = 23)
    
    if(! state %in% validStates) {
        stop("invalid state")
    }
    
    if(! outcomeName %in% names(validOutcomes)) {
        stop("invalid outcome")
    }
    
    # Filter by the relevant outcome
    outcomes <- data.frame("Hospital.Name" = outcomes[, 2],
                           "State" = outcomes[, 7],
                           "Outcome" =
                               as.numeric(outcomes[, validOutcomes[outcomeName]]),
                           stringsAsFactors = FALSE)
    
    # Filter by the intended state
    outcomes <- outcomes[outcomes[, "State"] == state, ]
    
    # Remove NAs
    outcomes <- outcomes[!is.na(outcomes[,3]), ]
    
    outcomes
}