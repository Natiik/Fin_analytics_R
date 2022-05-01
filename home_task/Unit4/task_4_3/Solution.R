print('---------------------------------------------------')
P <- list(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)
A <- as.list(c(2, 6, 12, 24))

getNot <- function(list0, list1) {
  result <- list()
  for (i in seq(list0)) {
    if (!list0[[i]] %in% list1) {
      temp <- list(list0[[i]])
      result <- append(result, temp)
    }
  }
  return(result)
}

print("NOT A:")
print(toString(getNot(P, A)))