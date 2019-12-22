convultion <- function(m1,m2,cl) 
{
  for(i in 1:(nrow(m1)-2)){
    for(j in 1:(nrow(m1)-2)){  
      m2[i,j] = sum(m1[i:(i+2),j:(j+2)] * cl)
    }
  }
  m2
}

spooling <- function(m1,m2) 
{
  for(m in 0:(nrow(m1)/2-1)){
    for(n in 0:(nrow(m1)/2-1)){  
      m2[m+1,n+1] = max(m1[(1+m*2):(2+m*2),(1+n*2):(2+n*2)])
    }
  }
  m2
}

x88 <- rbind(
c(1,0.7,0.7,1,0.7,0.7,0.5,0.2),
c(0.7,1,1,0.7,0.5,0.5,0.7,0.5),
c(0.5,0.7,0.7,0.5,0,0.2,0.5,0.7),
c(0.2,0.5,0,0,0,0.2,0.7,0.5),
c(0,0.2,0,0.2,0.2,0.2,0.7,0.7),
c(0,0,0,0.2,0.5,0.5,0.7,0.7),
c(0,0,0,0.2,0.5,0.7,0.7,1),
c(0,0.2,0.2,0.5,0.5,0.7,1,1)
)

convultionl1 <- rbind(
  c(1,0,-1),
  c(1,0,-1), 
  c(1,0,-1)
)

convultionl2 <- rbind(
  c(1,1,1),
  c(0,0,0), 
  c(-1,-1,-1)
)

convultionl3 <- rbind(
  c(1,0,0.5),
  c(0,0,0), 
  c(0,-0.5,0)
)

convultionl4 <- rbind(
  c(1,0,1),
  c(0,1,0), 
  c(1,0,1)
)

x661 <- matrix(0, 6, 6)
x662 <- matrix(0, 6, 6)

x331 <- matrix(0, 3, 3)
x332 <- matrix(0, 3, 3)

x661 =convultion(x88,x661,convultionl1)
x662 =convultion(x88,x662,convultionl2)

x331 =spooling(x661,x331)
x332 =spooling(x662,x332)


fc1 = sum(x331 * convultionl3)
fc2 = sum(x332 * convultionl4)

fc3 = 0.5*fc1 + 0.7*fc2
fc4 = 0.1*fc1 + 0.2*fc2

fc3
fc4
#3.58 0.98
