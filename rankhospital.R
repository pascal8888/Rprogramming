## Need to submit() for rankhospital part 4 of the assignment still
rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (state %in% data$State) {
        data[,11] <- as.numeric(data[,11])
        data[,17] <- as.numeric(data[,17])
        data[,23] <- as.numeric(data[,23])
        data <- data[, c(2, 7, 11, 17, 23)]
        unique_States <- sort(unique(data$State))
        for (i in 1:length(unique_States)) {
            statedata <- split(data, data$State)
        }
        if (num == "best" || num == "worst") {y <- 1} else {y <- num}
        if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") {
            if (outcome == "heart attack") {
                for (i in 1:length(unique_States)) {
                    statedata[[i]] <- statedata[[i]][order(statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,statedata[[i]]$Hospital.Name),]
                }
                if (num == "worst"){
                    for (i in 1:length(unique_States)) {
                        statedata[[i]] <- statedata[[i]][order(-statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,statedata[[i]]$Hospital.Name),]
                    }
                }
                ## find state
                final <- as.vector(c())
                s <- 55
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

            if (outcome == "heart failure") {
                for (i in 1:length(unique_States)) {
                    statedata[[i]] <- statedata[[i]][order(statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,statedata[[i]]$Hospital.Name),]
                }
                if (num == "worst"){
                    for (i in 1:length(unique_States)) {
                        statedata[[i]] <- statedata[[i]][order(-statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,statedata[[i]]$Hospital.Name),]
                    }
                }
                ## find state
                final <- as.vector(c())
                s <- 55
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

            if (outcome == "pneumonia") {
                for (i in 1:length(unique_States)) {
                    statedata[[i]] <- statedata[[i]][order(statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,statedata[[i]]$Hospital.Name),]
                }
                if (num == "worst"){
                    for (i in 1:length(unique_States)) {
                        statedata[[i]] <- statedata[[i]][order(-statedata[[i]]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,statedata[[i]]$Hospital.Name),]
                    }
                }
                ## find state
                final <- as.vector(c())
                s <- 55
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

        } else {
            stop("invalid outcome")
        }
    } else {
        stop("invalid state")

    }

}
