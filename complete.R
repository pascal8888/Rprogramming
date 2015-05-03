# options(error=recover)
complete <- function(directory, id = 1:332) {
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
y <- nrow(cleanData)
xID = c(1:332)
df <- as.data.frame(xID)
names(df)[1] <- "id"
df$nobs <- c(0)
for (elem in id) {
if (elem == 1) {tmp <- -117}
if (elem > 1) {tmp <- 0}
    for (j in 1:y) {
        if (cleanData[j,4] == elem) {
            tmp <- tmp + 1
            df$nobs[elem] <- tmp
        }
    }
}
result <- df[id,]
return(result)
}
