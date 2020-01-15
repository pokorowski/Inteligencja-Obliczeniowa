#-----------------------------------------------------------------
#POSTAOWE OPERACJE
library(e1071)
library(party)
library(class)
library(ggplot2)
library(dplyr)
library(neuralnet)

#ustaw folder roboczy
setwd("E:/InteligencjaObliczeniowaProjekt")
#pobierz dane z pliku adult.csv.
adult <- read.csv(file="adult.csv", header=TRUE, sep=",",strip.white = T)
#-----------------------------------------------------------------
#Czyszczenie, preprocessing danych

adult<-adult[,-3]

adult %>%
  summarise(
    workclass = sum(workclass == "?"),
    education = sum(education == "?"),
    marital.status = sum(marital.status == "?"),
    occupation = sum(occupation == "?"),
    relationship = sum(relationship == "?"),
    race = sum(race == "?"),
    sex = sum(sex == "?"),
    native.country = sum(native.country == "?")
  )

levels(adult$workclass)[1] <- 'Other/Unknown'
levels(adult$occupation)[1] <- 'Other/Unknown'
levels(adult$native.country)[1] <- 'Other/Unknown'

adult %>%
  summarise(
    workclass = sum(workclass == "?"),
    occupation = sum(occupation == "?"),
    native.country = sum(native.country == "?")
  )

adult$workclass <- gsub('^Federal-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^Local-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^State-gov', 'Government', adult$workclass) 

# combine into Sele-Employed job
adult$workclass <- gsub('^Self-emp-inc', 'Self-Employed', adult$workclass)
adult$workclass <- gsub('^Self-emp-not-inc', 'Self-Employed', adult$workclass)

# combine into Other/Unknown
adult$workclass <- gsub('^Never-worked', 'Other/Unknown', adult$workclass)
adult$workclass <- gsub('^Without-pay', 'Other/Unknown', adult$workclass)

#
adult$income <- gsub('^<=50K', 'low', adult$income)
adult$income <- gsub('^>50K', 'high', adult$income)

adult$workclass <- as.factor(adult$workclass)
adult$occupation <- as.factor(adult$occupation)
adult$native.country <- as.factor(adult$native.country)
adult$income <- as.factor(adult$income)
-----------------------------------------------------------------------------------------------------------------------
#Proste dane statystyczne np. srednia, odchylenie, min, max dla kazdej z kolumn

incomeFreq <- adult %>% count(income)
incomeFreq 
lowIncFreq <-as.numeric(incomeFreq[2,2])
highIncFreq <-as.numeric(incomeFreq[1,2])

generatePieChart <-function(freq,pieLabels,title) { 
  lbls <- pieLabels
  pct <- round(freq/sum(freq)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  pie(incGroupNumber,labels = lbls, col=rainbow(length(lbls)),
      main="Representing groups by income")
}

generatePieChart(c(lowIncFreq,highIncFreq),c("low","high"),"Representing groups by income")

#wektor zawierajacy indeksy kolumn zawierajacych dane liczbowe
numColumnsIndices <- c(1,4,10,11,12)

#nazwy kolumn zawierajacych dane liczbowe
nunColumnsHeaders <- colnames(adult)[numColumnsIndices]
#dlugosc wektora numColumnsIndices
nCIL <- length(numColumnsIndices)

basicDataFunctionsNames <- c("mean","min","max","standard.deviation")

meanAll<-function(csvData){
  means <- c(1:nCIL)*0
  for (i in 1:nCIL) {
    means[i] <- mean(csvData[,numColumnsIndices[i]])
  }
  return(means)
}

minAll<-function(csvData){
  mins <- c(1:nCIL)*0
  for (i in 1:nCIL) {
    mins[i] <- min(csvData[,numColumnsIndices[i]])
  }
  return(mins)
}

maxAll<-function(csvData){
  maxs <- c(1:nCIL)*0
  for (i in 1:nCIL) {
    maxs[i] <- max(csvData[,numColumnsIndices[i]])
  }
  return(maxs)
}

standardDevationAll<-function(csvData){
  sds <- c(1:nCIL)*0
  for (i in 1:nCIL) {
    sds[i] <- sd(csvData[,numColumnsIndices[i]])
  }
  return(sds)
}

functionsComparisonMatrix <- rbind(
  meanAll(adult),
  minAll(adult),
  maxAll(adult),
  standardDevationAll(adult)
)

colnames(functionsComparisonMatrix) <- nunColumnsHeaders
rownames(functionsComparisonMatrix) <- basicDataFunctionsNames 

functionsComparisonMatrix

write.csv(functionsComparisonMatrix,"ProbMatrix.csv")
#---------------------------------------------------------------------------------------------------------------
#Klasyfikacja
adultNum <- adult
stringColumnsIndices <- c(2,3,5,6,7,8,9,13)

stringDataToNumeric<-function(csvData){
 
  for (i in stringColumnsIndices) {
    csvData[,i] <- as.numeric(csvData[,i])
  }
  return(csvData)
}

adultNum <- stringDataToNumeric(adultNum)


ran <- sample(1:nrow(adult), 0.7 * nrow(adult)) 

##funkcja normalizacyjna
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##przeprowadz normalizacje dla kolumn numerycznych
adult_norm <- as.data.frame(lapply(adultNum[,1:13], nor))
#Ekstracja zbioru treningowego
adult_train <- adult_norm[ran,] 
#Ekstracja zbioru testowego
adult_test <- adult_norm[-ran,] 
#Ekstracja ostatniej kolumny zbioru treningowego
adult_train_category <- adultNum[ran,14]
#Ekstracja ostatniej kolumny zbioru treningowego
adult_test_category <- adultNum[-ran,14]


pr3 <- knn(adult_train,adult_test,cl=adult_train_category,k=3)
pr5 <- knn(adult_train,adult_test,cl=adult_train_category,k=5)
pr11 <- knn(adult_train,adult_test,cl=adult_train_category,k=11)

bayes <- naiveBayes(adult_train,adult_train_category)
adultNum[ran,]
myFormula <- income ~ .
adult_ctree <- ctree(myFormula, data=adultNum[ran,])
test_predict <- predict(adult_ctree,adultNum[-ran,],type="response")

##create confusion matrix
tab3 <- table(pr3,adult_test_category)
tab5 <- table(pr5,adult_test_category)
tab11 <- table(pr11,adult_test_category)
tab_bayes <- table(predict(bayes, adult_test), adult_test_category)
tab_tree <- table(test_predict,adultNum[-ran,]$income)

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
ggplot(data=df, aes(x=Clasificator, y=Accuracy, fill=Clasificator )) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Accuracy), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
#-------------------------------------------------------------------------------------------------
#Grupowanie
library(MLmetrics)
library(dbscan)
adultGroup <- adult
adultGroup<- stringDataToNumeric(adultGroup)


##przeprowadz normalizacje dla kolumn numerycznych
adultGroup_norm <- as.data.frame(lapply(adultGroup[,1:13], nor))
##przypisz zmiennej wartosci kolumny income
adultGroup_head <- adultGroup[,"income"]
adultGroupNum_head <- as.numeric(adultGroup_head)
groupResult<- kmeans(adultGroup_norm ,2)
groupResult$size
groupResult$centers 
groupResult$cluster 
table(groupResult$cluster,adultGroup_head)
MLmetrics::Accuracy(groupResult$cluster,adultGroupNum_head)

groupResultDbs <- dbscan::dbscan(adultGroup_norm, 0.8, 2)
MLmetrics::Accuracy(groupResultDbs$cluster,adultGroupNum_head)
#-------------------------------------------------------------------------------------------------
#Reguly asocjacyjne

library(arules)

stringColumnsRuleIndices <- c(2,3,5,6,7,8,9,13,14)

rulesAdult <- adult

rulesAdult <- rulesAdult %>% mutate(age = case_when(age >= 16  & age <= 24 ~ "adolescent",
                                             age > 24  & age <= 64 ~  "adult",
                                             age >= 64 ~ "senior"))
rulesAdult$age <- as.factor(rulesAdult$age)
summary(rulesAdult$age)

rulesAdult <- rulesAdult %>% mutate(hours.per.week = case_when( hours.per.week <= 20 ~ "0-20",
                                                    hours.per.week > 20  & hours.per.week <= 40 ~  "21-40",
                                                    hours.per.week > 40 ~ "40<"))
rulesAdult$hours.per.week<- as.factor(rulesAdult$hours.per.week)
summary(rulesAdult$hours.per.week)

 
rules <- apriori(adult[,stringColumnsRuleIndices],
                 parameter = list(minlen=2, supp=0.03, conf=0.70),
                 appearance = list(rhs=c("income=low", "income=high"),
                                   default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
#inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

inspect(subset(rules.sorted, subset = rhs %in% "income=high"))
