library(GA) 

k1 <- c(0,1,1)
k2 <- c(0,1,1)
k3 <- c(1,0,1)
k4 <- c(1,0,0)
k5 <- c(1,0,0)
k6 <- c(0,1,0)
k7 <- c(1,1,1)
kf <- rbind(k1,k2,k3,k4,k5,k6,k7)
len <- length(kf[,1])
z <- c(-3,-1,-2,-3,-1,-2,-4)
xvar <- 4 

xnor <- function(a,b){(a & b) | (!a & !b)}

maze <- function(x) { 
  war <- 0
  for(i in 1:len) {
     if(1 == max(xnor(x[z[i]],kf[i,]))){war = war + 1} 
  }
  war
}



gene <- ga(type="binary", 
           fitness=maze , 
           nBits=xvar, 
           maxiter=50,
           run=50,
           popSize=50
)

summary(gene)

maxFit <- gene@summary[,1]
meanFit <- gene@summary[,2]
generation <- 1:50
ticks <- seq(0,50,by=10)

all <- c(maxFit,meanFit)
up = max(all)
down = min(all)
plot(generation, maxFit,xaxt="n",ylim=c(down,up),
     type = "l", 
     col = "red", 
     lwd = 1,
     xlab = "pokolenie",
     ylab = "fitness(ocena)",
     main = "Dzialanie Alg. Genetycznego",
)  
axis(1, at=generation, labels = FALSE)
axis(1, at=ticks, labels = ticks, las = 2)
lines(generation, meanFit, col="blue", type = "l",  lwd=1)
legend(x = 37, y = down + 0.22, legend = c("maksymalnie","srednia"), col = c("red","blue"),lty=1,lwd=1)
