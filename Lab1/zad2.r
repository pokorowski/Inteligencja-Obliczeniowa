#a)
getwd()
#b)
osoby <- read.csv(file="osoby.csv", header=TRUE, sep=",")
print(osoby)
#c)
imiona = osoby[,c("imie")]
print(imiona)
#d)
kobiety <- subset( osoby, plec == "k")
print(kobiety)
#e)
mp50 <- subset(osoby, plec == "m" & wiek>50)
write.csv(mp50,"osoby2.csv")
#f)
mean <- mean(osoby$wiek)
print(mean)
#g)
osoby2 <- osoby
#cbind(osoby, wyplata=c(runif(20,0,50)))
osoby2$wyplata <- c(round((runif(nrow(osoby2),2000,5000)),2))
print(osoby2)
#h)
newRow <- data.frame(nazwisko="Kowalski",imie="Jan",plec= "m",wiek='30',wyplata='3000') 
osoby3 <- rbind(osoby2, newRow)
print(osoby3)
#i) 
write.csv(osoby3,"osoby3.csv")
