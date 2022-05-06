print('---------------------------------------------------')
table <- read.csv("home_task/test_work/input.csv")
amount <- as.numeric(table$amount)


nInterv <- 5

minValue <- min(amount)
maxValue <- max(amount)
step <- trunc((maxValue - minValue) / nInterv)
destribution <- list()
boards <- list()
for (i in 1:nInterv) {
  boards[i] <- minValue + (i - 1) * step
  n <- 0
  for (j in seq_along(amount)) {
    if ((i == nInterv) || (i == 1)) {
      if (i == 1) {
        if ((amount[j] >= (minValue + (i - 1) * step)) && (amount[j] <= (minValue + i * step))) {
          n <- n + 1
        }
      }else {
        if ((amount[j] > (minValue + (i - 1) * step)) && (amount[j] <= maxValue)) {
          n <- n + 1
        }
      }
    }else {
      if ((amount[j] > (minValue + (i - 1) * step)) && (amount[j] <= (minValue + i * step))) {
        n <- n + 1
      }
    }
  }
  destribution[[i]] <- n
}
boards[nInterv + 1] <- maxValue

getBottom <- function(list) {
  bottom <- list()
  for (i in 1:(length(list) - 1)) {
    bottom[[i]] <- list[[i]]
  }
  return(as.numeric(unlist(bottom)))
}

getUpper <- function(list) {
  upper <- list()
  for (i in 2:length(list)) {
    upper[[i]] <- list[[i]]
  }
  return(as.numeric(unlist(upper)))
}

getRelativeFr <- function(list, sum) {
  relative_fr <- list()
  for (i in seq(list)) {
    relative_fr[[i]] <- list[[i]] / sum
  }
  return(as.numeric(unlist(relative_fr)))
}

getCommulativeFr <- function(list) {
  comm <- list()
  comm[[1]]<-list[[1]]
  prev<-list[[1]]
  for (i in 2:length(list)){
    comm[[i]]<-prev+list[[i]]
    prev<-comm[[i]]
  }
  return(as.numeric(unlist(comm)))
}

table <- data.frame(
  from = getBottom(boards),
  to = getUpper(boards),
  frequency_n = as.numeric(unlist(destribution)),
  relative_fr = getRelativeFr(destribution, length(amount)),
  comulative_fr = getCommulativeFr(destribution)
)

write.csv(table, "home_task/test_work/Task2/distribution_output.csv")
print(table)
