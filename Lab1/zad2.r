getwd()
osoby <- read.csv(file="osoby.csv", header=TRUE, sep=",")
print(osoby)
imiona = osoby[,c("imie")]
print(imiona)
kobiety <- subset( osoby, plec == "k")
print(kobiety)
mp50 <- subset(osoby, plec == "m" && wiek>50)
write.csv(mp50,"osoby2.csv")
mean <- mean(osoby$wiek)

