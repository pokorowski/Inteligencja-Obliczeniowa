library(e1071)
library(party)
library(class)
library(ggplot2)
iris <- read.csv("diabetes.csv")

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


pr3 <- knn(iris_train,iris_test,cl=iris_target_category,k=3)
pr5 <- knn(iris_train,iris_test,cl=iris_target_category,k=5)
pr11 <- knn(iris_train,iris_test,cl=iris_target_category,k=11)

bayes <- naiveBayes(iris_train, iris_target_category)

myFormula <- class ~ .
iris_ctree <- ctree(myFormula, data=iris[ran,])
test_predict <- predict(iris_ctree,iris[-ran,],type="response")

##create confusion matrix
tab3 <- table(pr3,iris_test_category)
tab5 <- table(pr5,iris_test_category)
tab11 <- table(pr11,iris_test_category)
tab_bayes <- table(predict(bayes, iris_test), iris_test_category)
tab_tree <- table(test_predict,iris[-ran,]$class)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
ac3 = accuracy(tab3)
ac5 = accuracy(tab5)
ac11 = accuracy(tab11)
acb = accuracy(tab_bayes)
act = accuracy(tab_tree)

ac3
tab3

ac5
tab5

ac11
tab11

acb
tab_bayes

act
tab_tree

Accuracy <- c(ac3,ac5,ac11,acb,act)
Clasificator <- c("knn3","knn5","knn11","naive_bayes","decision_tree")
df <- data.frame(Clasificator,Accuracy)
ggplot(data=df, aes(x=Clasificator, y=Accuracy)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Accuracy), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
