library(stringr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    values <- numeric(0)
    
    for(fileNumber in str_pad(id, 3, pad = "0")) {
        data <- read.csv(paste0(directory, "/", fileNumber, ".csv"))
        values <- c(values, data[[pollutant]])
    }
    mean(values, na.rm = TRUE)
}