rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Begin state "if" statement - encapsulates all code - if the exact two-letter uppercase abbreviation for state is not found in the dataset return "invalid state".
    if (state %in% data$State) {
        data[,11] <- as.numeric(data[,11])
        data[,17] <- as.numeric(data[,17])
        data[,23] <- as.numeric(data[,23])
        data <- data[, c(2, 7, 11, 17, 23)]
        unique_States <- sort(unique(data$State))
        for (i in 1:length(unique_States)) {
            statedata <- split(data, data$State)
        }
        s <- 55  ## s will hold the state with the outcome value being sought.  If s does not get set to a number within - 1:length(unique_States) - it will return "NA"
        final <- as.vector(c())
        if (num == "best" || num == "worst") {y <- 1} else {y <- num}  ## y will hold the final position in the data frame for the ranked hospital.  best and worst is toggled to first postion in the data frame by using order and a "-" in front of the column name on the sort to make it decending order
## Begin the large if statement that checks to ensure the outcome is valid
        if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") {
 ## Begin specific outcome processing
            if (outcome == "heart attack") {
## Set sort order to ascending for each state for best and any specific number for "num"
                for (i in 1:length(unique_States)) {
                    statedata[[i]] <- statedata[[i]][order(statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,statedata[[i]]$Hospital.Name),]
                }
## Set sort order to descending for worst for each state
                if (num == "worst"){
                    for (i in 1:length(unique_States)) {
                        statedata[[i]] <- statedata[[i]][order(-statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,statedata[[i]]$Hospital.Name),]
                    }
                }
                ## Set s to state data frame index for heart attack ratings
                    for (i in 1:length(unique_States)) {
                    if (num == "worst" || num == "best") {num <- 1}
                    if (num > nrow(statedata[[i]])) {
                         final <- "NA"
                    } else {
                        if (!any(is.na(statedata[[i]][y,2])) && statedata[[i]][y,2] == state) {
                            s <- i
                        }
                    }
                }
                if (s < 55) {final<- statedata[[s]][y,1]}
            return(final)
            # End Heart Attack
            if (outcome == "heart failure") {
                for (i in 1:length(unique_States)) {
                    statedata[[i]] <- statedata[[i]][order(statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,statedata[[i]]$Hospital.Name),]
                }
                if (num == "worst"){
                    for (i in 1:length(unique_States)) {
                        statedata[[i]] <- statedata[[i]][order(-statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,statedata[[i]]$Hospital.Name),]
                    }
                }
                ## Set s to state data frame index for heart failure ratings
                for (i in 1:length(unique_States)) {
                    if (num == "worst" || num == "best") {num <- 1}
                    if (num > nrow(statedata[[i]])) {
                        final <- "NA"
                    } else {
                        if (!any(is.na(statedata[[i]][y,2])) && statedata[[i]][y,2] == state) {
                            s <- i
                        }
                    }
                }
                if (s < 55) {final<- statedata[[s]][y,1]}
                return(final)
            }
            ## End Heart Failure
            if (outcome == "pneumonia") {
                for (i in 1:length(unique_States)) {
                    statedata[[i]] <- statedata[[i]][order(statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,statedata[[i]]$Hospital.Name),]
                }
                if (num == "worst"){
                    for (i in 1:length(unique_States)) {
                        statedata[[i]] <- statedata[[i]][order(-statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,statedata[[i]]$Hospital.Name),]
                    }
                }
                ## Set s to state data frame index for pneumonia ratings
                for (i in 1:length(unique_States)) {
                    if (num == "worst" || num == "best") {num <- 1}
                    if (num > nrow(statedata[[i]])) {
                        final <- "NA"
                    } else {
                        if (!any(is.na(statedata[[i]][y,2])) && statedata[[i]][y,2] == state) {
                            s <- i
                        }
                    }
                }
                if (s < 55) {final<- statedata[[s]][y,1]}
                return(final)
            }
            ## End Pneumonia
## End specific outcome processing
        } else {
## End the large if statement that checks to ensure the outcome is valid
            stop("invalid outcome")
        }
    } else {
## End state "if" statement - encapsulates all code - if the exact two-letter uppercase abbreviation for state is not found in the dataset return "invalid state"
        stop("invalid state")
    }
}
}
