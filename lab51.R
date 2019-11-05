
iris <- read.csv("iris.csv")

set.seed(1234)
ind <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]