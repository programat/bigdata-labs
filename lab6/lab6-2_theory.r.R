# lab6-2_theory.R

if (!require("klaR")) install.packages("klaR")
library(klaR)

naive_iris <- NaiveBayes(iris$Species ~ ., data = iris)
naive_iris$tables
naive_iris$tables$Petal.Width

naive_iris


opar=par()
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(naive_iris,lwd = 2, legendplot=TRUE)
legend("topleft", legend=c("setosa", "versicolor", "virginica"),lty=1:3, cex=0.5)

par = opar
pred <- predict(naive_iris, iris[, -5])$class
table(Fact = iris$Species, Pred = pred)
pred

Acc <- mean(pred == iris$Species)
Acc

paste0("Accuracy: ", round(100 * Acc, 2), "%", sep='')


# clussification decision tree

# data preparation
set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainData <- iris[ind == 1,]
testData <- iris[ind == 2,]
nrow(trainData)
nrow(testData)
nrow(iris)

# buioding the model
if (!require("party")) install.packages("party")
library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data = trainData)

table(predict(iris_ctree), trainData$Species)
plot(iris_ctree)


test_predicted <- predict(iris_ctree, newdata=testData)
table(test_predicted, testData$Species)


if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

rf <- randomForest(Species ~ ., data = trainData, ntree = 100, proximity = TRUE)
table(predict(rf), trainData$Species)
print(rf)

