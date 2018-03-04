source("filterOutcomes.R")

rankhospital <- function(state, outcomeName, num = "best") {
    outcomes <- filteredOutcomes(state, outcomeName)
    
    # Get the specified ranked hospital name
    if(num == "best") {
        outcomes[order(outcomes[,3], outcomes[,1]), ][1, 1]
    } else if(num == "worst") {
        outcomes[order(outcomes[,3],
                       outcomes[,1]), ][nrow(outcomes), 1]
    } else {
        outcomes[order(outcomes[,3], outcomes[,1]), ][num, 1]
    }
}