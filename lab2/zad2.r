library(GA) 

p <- c(100,300, 200, 40, 500, 70, 100,250,300,280,300)
w <- c(7, 7, 6, 2, 5, 6, 1,3,10,3,15) 
W <- 25 
n <- length(p) 

knapsack <- function(x) { 
  war <- sum(x * p)
  wag <- sum(x * w)
  if(wag > W){
    0
  }else{
    war
  }
}

gene <- ga(type="binary", 
           fitness=knapsack , 
           nBits=n, 
           maxiter=50,
           run=50,
           popSize=100
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
legend(x = 37, y = 545, legend = c("maksymalnie","srednia"), col = c("red","blue"),lty=1,lwd=1)
