corr <- function(directory, threshold = 0) {
x <- as.list(directory)
completeOBS <- do.call("complete", x, quote=FALSE)
vCor <- c(0)
file_list <- list.files(directory, full.names = TRUE)
for (i in 1:332) {
    data <- read.csv(file_list[i])
    cdata <- data[complete.cases(data),]
    if (completeOBS[i,2] > threshold) {
        vCor[i] <- cor(cdata$sulfate[i],cdata$nitrate[i],use="complete.obs")
    }
}
vCor <- vCor[!is.na(vCor)]
return(vCor)
}
