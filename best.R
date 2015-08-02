best <- function(state, outcome) {
    if (outcome == "heart attack") {
        column = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        column = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        column = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("invalid outcome")
    }
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (state %in% df$State)
    {
        j <- df[df$State == state, c("Hospital.Name", column)]
        index <- order(as.numeric(j[, column]), j[, "Hospital.Name"])
        as.character(j[index[1], "Hospital.Name"])
    } else {
        stop("invalid state")
    }
}