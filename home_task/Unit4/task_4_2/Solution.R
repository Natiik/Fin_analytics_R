print('---------------------------------------------------')
X <- as.list(c(1, 3, 5, 7, 8, 9))
Y <- as.list(c(2, 4, 7, 9))
Z <- as.list(c(1, 2, 3, 4, 7))

getUnion <- function(list1, list2) {
  result <- as.list(list1)
  for (i in seq(list2)) {
    if (!list2[[i]] %in% list1) {
      temp <- as.list(list2[[i]])
      result <- append(result, temp)
    }
  }
  return(result)
}

getIntersection <- function (list1, list2){
  result <- list()
  for (i in seq(list2)) {
    if (list2[[i]] %in% list1) {
      temp <- as.list(list2[[i]])
      result <- append(result, temp)
    }
  }
  return (result)
}

print("RESULTS")
print("                          ")
print("a)")
print(toString(getUnion(X, Z)))

print("                          ")
print("b)")
print(toString(getIntersection(X, Y)))

print("                          ")
print("c)")
print(toString(getIntersection(X, Z)))

print("                          ")
print("d)")
print(toString(getUnion(X, getUnion(Y, Z))))

print("                          ")
print("e)")
print(toString(getIntersection(X, getIntersection(Y, Z))))

print("                          ")
print("f)")
print(toString(getIntersection(getUnion(X, Y), Z)))

print("                          ")
print("g)")
print(toString(getUnion(getIntersection(Y, Z), getIntersection(X, Y))))


print("                          ")
print("h)")
print(toString(getUnion(X, Y)))

print("                          ")
print("i)")
print(toString(getIntersection(Y, X)))