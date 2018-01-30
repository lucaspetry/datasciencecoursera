source("complete.R")

corr <- function(directory, threshold = 0) {
    data <- complete(directory)
    ids <- data[which(data$nobs > threshold),]$id
    res <- numeric(0)
    
    for(fileNumber in ids) {
        data <- read.csv(paste0(directory, "/",
                                str_pad(fileNumber, 3, pad = "0"), ".csv"))
        
        res <- c(res, cor(data$sulfate, data$nitrate, use="complete.obs"))
    }
    res
}