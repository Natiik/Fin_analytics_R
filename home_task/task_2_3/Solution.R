print('---------------------------------------------------')
table <- read.csv("home_task/task_2_3/input.csv")

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
comulative_fr <- list()
count <- getSum(frequency)

for (i in seq(bottom)) {
  midpoint[[i]] <- trunc((bottom[[i]] + up[[i]]) / 2)
  relative_fr[[i]] <- round(frequency[[i]] / count,digits=3)
  if (i == 1) {
    comulative_fr[[i]] <- relative_fr[[i]]
  }else {
    comulative_fr[[i]] <- comulative_fr[[i - 1]] + relative_fr[[i]]
  }
}

result<- data.frame(
  from=as.numeric(unlist(bottom)),
  to=as.numeric(unlist(up)),
  frequency=as.numeric(unlist(frequency)),
  midpoint=as.numeric(unlist(midpoint)),
  relative_frequency=as.numeric(unlist(relative_fr)),
  comulative_frequency=as.numeric(unlist(comulative_fr))
)
print(result)
write.csv(result,"home_task/task_2_3/output.csv")
print("DONE")