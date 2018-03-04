source("filterOutcomes.R")

best <- function(state, outcomeName) {
    outcomes <- filteredOutcomes(state, outcomeName)
    
    # Get the best
    outcomes[order(outcomes[,3], outcomes[,1]), ][1,1]
}