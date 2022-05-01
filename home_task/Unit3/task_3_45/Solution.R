print('---------------------------------------------------')
table <- read.csv("input.csv")

get_range <- function(numeric) {
  return(max(numeric) - min(numeric))
}

get_interquartile <- function(numeric) {
  return(quantile(numeric, prob = .75) - quantile(numeric, prob = .25))
}

find_capacity_by_location <- function(name) {
  location <- as.list(table$Location)
  for (i in seq(location)) {
    if (location[[i]] == name) {
      return(as.list(table$Capacity)[[i]])
    }
  }
}

capacity <- as.numeric(table$Capacity)
size <- as.numeric(length(lengths(as.list(table$Capacity))))
mean <- mean(capacity, trim = 0, na_rm = true)
median <- median(capacity, na_rm = true)
range <- get_range(capacity)
interquartile <- get_interquartile(capacity)
variance <- var(capacity) * ((size - 1) / size)
st_deviation <- sqrt(variance)
z1 <- (find_capacity_by_location("Pascagoula Mississippi")-mean)/st_deviation
z2 <- (find_capacity_by_location("Texas City Texas")-mean)/st_deviation


result <- data.frame(key=rep("", 0), value=rep(NA, 0), stringsAsFactors=FALSE)
result <- rbind(data.frame(key=("mean"), value=mean), result)
result <- rbind(data.frame(key=("median"), value=median), result)
result <- rbind(data.frame(key=("range"), value=range), result)
result <- rbind(data.frame(key=("interquartile"), value=interquartile), result)
result <- rbind(data.frame(key=("st_deviation"), value=st_deviation), result)

var_res <- data.frame(key=rep("", 0), value=rep(NA, 0), stringsAsFactors=FALSE)
var_res <- rbind(data.frame(key=("variance"), value=variance), var_res)

d_res <- data.frame(key=rep("", 0), value=rep(NA, 0), stringsAsFactors=FALSE)
d_res <- rbind(data.frame(key=("Pascagoula Z"), value=z1), d_res)
d_res <- rbind(data.frame(key=("Texas Z"), value=z2), d_res)

print(result)
print("                                             ")
print(var_res)
print("                                             ")
print(d_res)

write.csv(result, "output.csv")
write.csv(d_res, "output_d.csv")
write.csv(var_res, "output_var.csv")

print("DONE")
