print('---------------------------------------------------')
S <- 6
d <- 2
select <- 3

getFactorial <- function(n) {
  result <- 1
  for (i in seq(n)) {
    result <- result * i;
  }
  return(result)
}

sample_space <- getFactorial(S) / (getFactorial(select) * getFactorial(S-select))
probability <- (S*d)/sample_space
print("SAMPLE SPACE LENGTH:")
print(sample_space)
print("                                 ")
print("PROBABILITY")
print(probability)