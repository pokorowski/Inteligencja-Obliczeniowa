#zad1
#cat("zad1\n")

#a)
a = 45*678
print(a)
#cat("a)\na=",a,"\n")

#b)
x <- c(7,4,2,0,9)
y <- c(2,1,5,3,3)
print(x)
print(y)

#c)
c <- x+y
print(c)

#d)
d <- x*y
print(d)

#e)
e = sum(d)
print(e)

#f)
f1 = matrix(c(0,2,1,1,6,4,5,0,3),nrow=3,ncol=3,byrow = TRUE)
f2 = matrix(c(9,8,7,1,2,7,4,9,2),nrow=3,ncol=3,byrow = TRUE)
f3 = f1 %*% f2	
print(f1)
print(f2)
print(f3)

#g
g <- 1:100
print(g)

#h1

h.sum = sum(g)
h.prod = prod(g)
h.sd = sd(g)
print(h.sum)
print(h.prod)
print(h.sd)

#i)
#example with only integers
#i <- sample(0:50, 20, replace=T) 
i <- runif(20,0,50)
print(i)

#j)
j.min = min(i)
j.max = max(i)
print(j.min)
print(j.max)
