print('---------------------------------------------------')
library("stats")
table <- read.csv("home_task/test/input.csv")

get_interquartile <- function(numeric) {
  return(quantile(numeric, prob = .75) - quantile(numeric, prob = .25))
}

input<- as.numeric(table$дохід)
mean <- mean(input, trim = 0, na_rm = true)
mad <- mad(input);
size <- as.numeric(length(lengths(as.list(table$дохід))))
variance <- var(input) * ((size - 1) / size)
st_deviation <- sqrt(variance)
interquartile <- get_interquartile(input)
z1 <- (5353307-mean)/st_deviation
q1 <-quantile( input, prob = .25)
q3 <- quantile(input,prob = .75 )
print(mean)
print(mad)
print(size)
print(variance)
print(st_deviation)
print(interquartile)
print(z1)
print(q1)
print(q3)
