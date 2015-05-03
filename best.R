best <- function(state, outcome) {
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
if (state %in% data$State) {
    if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") {
        if (outcome == "heart attack") {
            state_range <- as.vector(which(data[7] ==state))
            data[,11] <- as.numeric(data[,11])
            state_sub <- data[state_range,]
            x <- min(state_sub[11][complete.cases(state_sub[11]),])
            indx <- which(state_sub[,11]==x)
            return(state_sub$Hospital.Name[indx])
        }
        if (outcome == "heart failure") {
            state_range <- as.vector(which(data[7] ==state))
            data[,17] <- as.numeric(data[,17])
            state_sub <- data[state_range,]
            x <- min(state_sub[17][complete.cases(state_sub[17]),])
            indx <- which(state_sub[,17]==x)
            return(state_sub$Hospital.Name[indx])
        }
        if (outcome == "pneumonia") {
            state_range <- as.vector(which(data[7] ==state))
            data[,23] <- as.numeric(data[,23])
            state_sub <- data[state_range,]
            x <- min(state_sub[23][complete.cases(state_sub[23]),])
            indx <- which(state_sub[,23]==x)
            return(state_sub$Hospital.Name[indx])
        }
    } else {
        stop("invalid outcome")
    }
    } else {
        stop("invalid state")

}

}

