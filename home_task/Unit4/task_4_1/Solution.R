print('---------------------------------------------------')
table <- read.csv("home_task/Unit4/task_4_1/sample_space.csv")

countLowerCase <- function(text) {
  charts <- as.list(strsplit(text, "")[[1]])
  n <- 0;
  for (i in seq(charts)) {
    if (tolower(charts[i]) == charts[i]) {
      n <- n + 1;
    }
  }
  return(n);
}


sample_space_list <- list("AB", "AC", "BC", "ab", "ac", "bc", "Aa", "Bb", "Cc", "Ab", "Ac", "Ba", "Bc", "Ca", "Cb")
cases <- 0;
for (i in seq(sample_space_list)) {
  if (countLowerCase(sample_space_list[[i]]) == 1) {
    cases <- cases + 1;
  }
}

probability <- cases/length(sample_space_list)

print("SAMPLE SPACE")
print("                               ")
print("Size")
print(length(sample_space_list))
print("                               ")
print("As table:")
print(table)
print("                               ")
print("As list")
print(toString(sample_space_list))
print("                               ")
print("Probability of selectiong exactly one defect:")
print(probability)
