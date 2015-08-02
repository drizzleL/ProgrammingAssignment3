rankall <- function (outcome, num = "best") {
    if (outcome == "heart attack") {
        column = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        column = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        column = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")
    }
    
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "factor")
    keys <- levels(df$State)
    values <- NULL
    for (state in keys) {
        j <- df[df$State == state, c("Hospital.Name", column)]
        index <- order(as.numeric(as.character(j[, column])), j[, "Hospital.Name"], na.last = NA)
        if (num == "best") {
            result <- as.character(j[index[1], "Hospital.Name"])
        } else if (num == "worst") {
            result <- as.character(j[tail(index, 1), "Hospital.Name"])
        } else {
            result <- as.character(j[index[num], "Hospital.Name"])
        }
        values <- c(values, result)
    }
    data.frame(hospital = values, state = keys, row.names = keys)
}