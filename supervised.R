data <- read.csv("data/hfd.csv")
unlabelled_data <- data[, 1:4]
labels <- data[, 5]
library("class")

#K nearest neighbours

k <- 17
k_summary <- cbind(k,p)
colnames(k_summary) <- c("k","% misclassified")
for(i in k){
  results <- knn.cv(unlabelled_data, cl=labels, k=i)
  class_agree <- table(results, labels)
  sum_agree <- sum(diag(class_agree))
  k_summary[i, 2] <- (19321 - sum_agree) / 19321
  sprintf("%d done", i)
}
print(k_summary)
print(class_agree)

plot(k_summary)
abline(v=17, col="blue")
min(k_summary[0:299,2])


#LDA and QDA
install.packages("QuantPsyc")
library(QuantPsyc)
x <- split(data, data$gate)
one <- x$"1"[1:4]
one
one_norm <- mult.norm(one)
one_norm$mult.test

two <- x$"2"[1:4]
two
two_norm <- mult.norm(two)
two_norm$mult.test

three <- x$"3"[1:4]
three
three_norm <- mult.norm(three)
three_norm$mult.test

four <- x$"4"[1:4]
four
four_norm <- mult.norm(four)
four_norm$mult.test

summary(x)

install.packages("MVN")
MVN::mvn(four)
