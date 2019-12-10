library(e1071)
library(party)
library(class)
library(ggplot2)
library(neuralnet)


iris <- read.csv("diabetes.csv")

nor <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}

ran <- sample(1:nrow(iris), 0.67 * nrow(iris)) 

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 8 coulumns of dataset because they are the predictors

iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4,5,6,7,8)], nor))

iris_train <- iris_norm[ran,] 
##extract testing set
iris_test <- iris_norm[-ran,] 
##extract 9th column of train dataset because it will be used as 'cl' argument in knn function.
iris_target_category <- iris[ran,9]
##extract 9th column if test dataset to measure the accuracy
iris_test_category <- iris[-ran,9]

class <- iris_target_category
temp <- cbind(iris_train,class)
temp$positive <- temp$class == "tested_positive"
temp$negative <- temp$class == "tested_negative"

class <- iris_test_category
class
temp1 <- cbind(iris_test,class)
temp1$positive <- temp1$class == "tested_positive"
temp1$negative <- temp1$class == "tested_negative"
temp1
network = neuralnet(positive + negative ~ pregnanttimes+glucoseconcentr+bloodpressure+skinthickness+insulin+massindex+pedigreefunc+age, temp, hidden=6,linear.output=TRUE)
res <- compute(network, iris_test)
prediction <- res$net.result

colnames(prediction ) <- c("positive","negative")
prediction [,1] <- round(prediction[,1])
prediction [,2] <- round(prediction[,2])
prediction
actual <- cbind(temp1[,10],temp1[,11])
actual[,1] <- as.numeric(actual[,1])
actual[,2] <- as.numeric(actual[,2]) 
confusionMatrix <- table(actual,prediction)
accuracy(confusionMatrix)
confusionMatrix


