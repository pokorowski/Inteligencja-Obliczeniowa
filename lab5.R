#repl.it/python

df <- read.csv("iris.csv")

sepallength <- df$sepallength
sepalwidth <- df$sepalwidth
petallength<- df$petallength
petalwidth <- df$petalwidth
class <- df$class

myPredictRow <- function(sl,sw,pl,pw) {
  if (sl < 6 & sw <4.5 & pl<2 & pw < 0.7) {
    return('Iris-setosa')
  } else {
    if (sl < 7.1 & sw <3.4 & pl<5.2 & pw < 1.9) {
      return('Iris-versicolor')
    } else {
      return('Iris-virginica')
    }
  }
}

count = 0
for(i in 1:150){
  if(class[i] == myPredictRow(sepallength[i],sepalwidth[i],petallength[i],petalwidth[i])){count = count + 1}
}
count/150 * 100