createDataSetFromFiles <- function(directory) {
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
    cleanData <- dataset[complete.cases(dataset),]
}
