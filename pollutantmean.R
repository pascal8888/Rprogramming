pollutantmean <- function(directory, pollutant, id = 1:332) {
file_list <- list.files(directory, full.names = TRUE)
for (file in file_list){
    if (!exists("dataset")){
        dataset <- read.csv(file)
    }
    if (exists("dataset")){
        temp_dataset <-read.csv(file)
        dataset<-rbind(dataset, temp_dataset)
        rm(temp_dataset)
    }
}
if (pollutant == "sulfate") {
    j <- 2
}
if (pollutant == "nitrate") {
    j <- 3
}
cleanData <- dataset[complete.cases(dataset[j]),]
cleanDataSubSet <- data.frame(Date= integer(0), nitrate= integer(0), sulfate = integer(0), ID = integer(0))
for (elem in id) {
    cleanDataSubSet  <- rbind((subset(cleanData, ID == elem, select = pollutant)),cleanDataSubSet)
}
x <- mean(cleanDataSubSet[[pollutant]])
round(x,digits=3)
}

