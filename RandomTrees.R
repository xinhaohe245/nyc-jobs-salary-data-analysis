rm(list=ls())
data <- read.csv("redone_data_analysis6.csv")
set.seed(1)
data$School.Level <- as.factor(data$School.Level)
data$Agency <- as.factor(data$Agency)
data$level_factor <- as.factor(data$level_factor)
data$Salary.Range.To <- (data$Salary.Range.To - mean(data$Salary.Range.To))/sd(data$Salary.Range.To)
data$Salary.Range.From <- (data$Salary.Range.From - mean(data$Salary.Range.From))/sd(data$Salary.Range.From)
train <- sample(nrow(data), nrow(data)*7/10)

# Bagging
library(randomForest)
bagged_trees <- randomForest(Salary.Range.To ~.-Salary.Range.To-Salary.Range.From, data,
                           subset = train, importance = TRUE)

pred <- predict(bagged_trees, newdata = data[-train, ])
test <-data[-train, "Salary.Range.To"]
plot(test, pred)
abline(0, 1)
tree_mse <- mean((pred - test)^2)
tree_rss <- sum((pred - test)^2)
tree_tss <- sum((test - mean(test))^2)
1 - tree_rss/tree_tss
tree_mse

