print('---------------------------------------------------')
table <- read.csv("/home_task/test_work/Task1/input.csv")
amount <- as.numeric(table$amount)

get_range <- function(numeric) {
  return(max(numeric) - min(numeric))
}

get_interquartile <- function(numeric) {
  return(quantile(numeric, prob = .75) - quantile(numeric, prob = .25))
}

size <- as.numeric(length(lengths(as.list(table$amount))))
mean <- mean(amount, trim = 0, na_rm = true)
median <- median(amount, na_rm = true)
range <- get_range(amount)
interquartile <- get_interquartile(amount)
variance <- var(amount) * ((size - 1) / size)
st_deviation <- sqrt(variance)

result <- data.frame(key=rep("", 0), value=rep(NA, 0), stringsAsFactors=FALSE)
result <- rbind(data.frame(key=("mean"), value=mean), result)
result <- rbind(data.frame(key=("median"), value=median), result)
result <- rbind(data.frame(key=("range"), value=range), result)
result <- rbind(data.frame(key=("interquartile"), value=interquartile), result)
result <- rbind(data.frame(key=("st_deviation"), value=st_deviation), result)

var_res <- data.frame(key=rep("", 0), value=rep(NA, 0), stringsAsFactors=FALSE)
var_res <- rbind(data.frame(key=("variance"), value=variance), var_res)

print(result)
print("                                             ")
print(var_res)
print("                                             ")

write.csv(result, "home_task/test_work/Task1/output.csv")
write.csv(var_res, "home_task/test_work/Task1/output_var.csv")

print("DONE")