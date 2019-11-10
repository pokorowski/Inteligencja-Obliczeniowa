library(party)

iris <- read.csv("iris.csv")
set.seed(1234)
#a)divideData
ind <- sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
#b)trainTree
myFormula <- class ~ sepallength + sepalwidth + petallength + petalwidth
iris_ctree <- ctree(myFormula, data=trainData)
train_predict <- predict(iris_ctree,trainData,type="response")
#c1)textTree
print(iris_ctree)
#c2)graphicTree
plot(iris_ctree)
#d)errorRate
mean(train_predict != trainData$class) * 100
#e)confusionMatrix
table(train_predict,trainData$class)
