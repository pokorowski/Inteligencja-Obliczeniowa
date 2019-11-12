
iris <- read.csv("diabetes.csv")

ran <- sample(1:nrow(iris), 0.67 * nrow(iris)) 

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4,5,6,7,8)], nor))

summary(iris_norm)

iris_train <- iris_norm[ran,] 
##extract testing set
iris_test <- iris_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
iris_target_category <- iris[ran,9]
##extract 5th column if test dataset to measure the accuracy
iris_test_category <- iris[-ran,9]
##load the package class
library(class)
##run knn function
pr3 <- knn(iris_train,iris_test,cl=iris_target_category,k=3)
pr5 <- knn(iris_train,iris_test,cl=iris_target_category,k=5)
pr11 <- knn(iris_train,iris_test,cl=iris_target_category,k=11)
##create confusion matrix
tab3 <- table(pr3,iris_test_category)
tab5 <- table(pr5,iris_test_category)
tab11 <- table(pr11,iris_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab3)
accuracy(tab5)
accuracy(tab11)

library(e1071)
repeating_sequence=rep.int(seq_len(nrow(irus)), irysf$Freq) #This will repeat each combination equal to the frequency of each combination

Titanic_dataset=Titanic_df[repeating_sequence,]
