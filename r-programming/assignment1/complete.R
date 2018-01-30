complete <- function(directory, id = 1:332) {
    ids <- integer(0)
    nobs <- integer(0)
    
    for(fileNumber in id) {
        data <- read.csv(paste0(directory, "/",
                                str_pad(fileNumber, 3, pad = "0"), ".csv"))
        cases <- complete.cases(data)
        ids <- c(ids, fileNumber)
        nobs <- c(nobs, length(cases[cases == TRUE]))
    }
    data.frame(id = ids, nobs = nobs)
}