print('---------------------------------------------------')
P <- 20
selected <- 6

getFactorial <- function(n) {
  result <- 1
  for (i in seq(n)) {
    result <- result * i;
  }
  return(result)
}

print("Number of combinations")
print(getFactorial(P) / (getFactorial(selected) * getFactorial(P - selected)))