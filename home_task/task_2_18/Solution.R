print('---------------------------------------------------')
table <- read.csv("home_task/task_2_18/input.csv")

bottom <- as.list(table$from)
up <- as.list(table$to)
frequency <- as.list(table$frequency)

getSum <- function(list) {
  sum <- 0
  for (i in seq(list)) {
    sum <- sum + list[[i]]
  }
  return(sum)
}

midpoint <- list()
relative_fr <- list()
cumulative_fr <- list()
count <- getSum(frequency)

for (i in seq(bottom)) {
  midpoint[[i]] <- trunc((bottom[[i]] + up[[i]]) / 2)
  relative_fr[[i]] <- round(frequency[[i]] / count,digits=3)
  if (i == 1) {
    cumulative_fr[[i]] <- relative_fr[[i]]
  }else {
    cumulative_fr[[i]] <- cumulative_fr[[i - 1]] + relative_fr[[i]]
  }
}

result<- data.frame(
  from=as.numeric(unlist(bottom)),
  to=as.numeric(unlist(up)),
  frequency=as.numeric(unlist(frequency)),
  midpoint=as.numeric(unlist(midpoint)),
  relative_frequency=as.numeric(unlist(relative_fr)),
  cumulative_frequency=as.numeric(unlist(cumulative_fr))
)
print(result)
write.csv(result,"home_task/task_2_18/output.csv")
print("DONE")