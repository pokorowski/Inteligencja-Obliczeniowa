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
          maxiter=100,
          run=100,
          popSize=100
          )

summary(gene)
