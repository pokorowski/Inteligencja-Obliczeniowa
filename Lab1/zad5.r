ludnosc <- read.csv(file="ludnosc.csv", header=TRUE, sep=",")
#Rok <- c(1900,1925,1939,1946,1950,1960,1970,1980,1990,2000,2010)
#Gdansk <- c(170,210,250,117,194,286,365,456,465,462,460)
#Poznan <- c(110,220,274,267,320,408,471,552,590,574,555)
#Szczecin <- c(210,254,287,72,178,269,338,388,413,416,405)
Rok <- ludnosc$Rok
Gdansk <- ludnosc$Gdansk
Poznan <- ludnosc$Poznan
Szczecin <- ludnosc$Szczecin
all <- c(Gdansk,Poznan,Szczecin)
up = max(all)
down = min(all)
plot(Rok, Gdansk,xaxt="n",ylim=c(down,up),
     type = "o", 
     col = "red", 
     lwd = 1,
     xlab = "Lata",
     ylab = "Liczba ludnosci [w tys:]",
     main = "Ludnosc w miastach polski",
    log = "x")  
axis(1, at=Rok, las=2)
lines(Rok, Poznan, col="yellow", type = "o",  lwd=1)
lines(Rok, Szczecin, col="green", type = "o",  lwd=1)
legend(x = 1985, y = 150, legend = c("Gdansk","Poznan","Szczecin"), col = c("red","yellow","green"),lty=1,lwd=1)
