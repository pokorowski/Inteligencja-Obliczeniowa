library(neuralnet)
library(plyr)
iris <- read.csv("iris.csv")
normalize <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
iris$sepal.length <- normalize(iris$sepal.length)
iris$sepal.width <- normalize(iris$sepal.width)
iris$petal.length<- normalize(iris$petal.length)
iris$petal.width <- normalize(iris$petal.width)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))

trainset <- iris[ind == 1,]
trainset$setosa = trainset$variety == "Setosa"
trainset$versicolor = trainset$variety == "Versicolor"
trainset$virginica = trainset$variety == "Virginica"

testset <- iris[ind == 2,]
testset$setosa = testset$variety == "Setosa"
testset$versicolor = testset$variety == "Versicolor"
testset$virginica = testset$variety == "Virginica"

network = neuralnet(setosa + versicolor + virginica  ~ sepal.length + sepal.width + petal.length + petal.width, trainset, hidden=3)

temp_test <- subset(testset, select = c("sepal.length","sepal.width", "petal.length", "petal.width"))
res <- compute(network, temp_test)
prediction <- res$net.result


actual <- cbind(testset[,6],testset[,7],testset[,8])
actual[,1] <- as.numeric(actual[,1])
actual[,2] <- as.numeric(actual[,2])
actual[,3] <- as.numeric(actual[,3])
colnames(prediction ) <- c("setosa"," Versicolor","Virginica")
prediction [,1] <- round(prediction[,1])
prediction [,2] <- round(prediction[,2])
prediction [,3] <- round(prediction[,3])

results <- data.frame(actual,prediction)
table(actual,prediction)