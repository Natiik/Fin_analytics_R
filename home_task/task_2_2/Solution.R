print('---------------------------------------------------')
data <- c(57, 51, 53, 52, 50, 60, 51, 51, 52, 52, 44, 53, 45, 57, 39, 53, 58, 47, 51, 48, 49, 49, 44, 54, 46, 52, 55, 54, 47, 53, 49, 52, 49, 54, 57, 52, 52, 53, 49, 47, 51, 48, 55, 53, 55, 47, 53, 43, 48, 46, 54, 46, 51, 48, 53, 56, 48, 47, 49, 57, 55, 53, 50, 47, 57, 49, 43, 58, 52, 44, 46, 59, 57, 47, 61, 60, 49, 53, 41, 48, 59, 53, 45, 45, 56, 40, 46, 49, 50, 57, 47, 52, 48, 50, 45, 56, 47, 47, 48, 46)
nInterv <- trunc(1 + log2(length(data)))
minValue <- min(data)
maxValue <- max(data)
step <- trunc((maxValue - minValue) / nInterv)
destribution <- list()
boards <- list()
for (i in 1:nInterv) {
  boards[i] <- minValue + (i - 1) * step
  n <- 0
  for (j in seq_along(data)) {
    if ((i == nInterv) || (i == 1)) {
      if (i == 1) {
        if ((data[j] >= (minValue + (i - 1) * step)) && (data[j] <= (minValue + i * step))) {
          n <- n + 1
        }
      }else {
        if ((data[j] > (minValue + (i - 1) * step)) && (data[j] <= maxValue)) {
          n <- n + 1
        }
      }
    }else {
      if ((data[j] > (minValue + (i - 1) * step)) && (data[j] <= (minValue + i * step))) {
        n <- n + 1
      }
    }
  }
  destribution[[i]] <- n
}
boards[nInterv + 1] <- maxValue
png("home_task/task_2_2/histogram.png")
hist(data, breaks = as.numeric(unlist(boards)), col = 'blue')
dev.off()

getBottom<-function(list){
  bottom<-list()
  for (i in 1:(length(list)-1)){
    bottom[[i]]<-list[[i]]
  }
  return(as.numeric(unlist(bottom)))
}

getUpper<-function(list){
  upper<-list()
  for (i in 2:length(list)){
    upper[[i]]<-list[[i]]
  }
  return(as.numeric(unlist(upper)))
}

table<-data.frame(
  from = getBottom(boards),
  to = getUpper(boards),
  frequency_n =as.numeric(unlist(destribution))
)

write.csv(table,"home_task/task_2_2/distribution_output.csv")
print(table)

