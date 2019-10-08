sumuj<-function(a,b){
s<-a+b 
return(s)
}

losuj<-function(a,b){
s <- runif(1,a,b)
return(s)
}

standaryzuj<-function(a){
s <- scale(a)
return(s)
}

normalizuj<-function(a){
s <-(a-min(a))/(max(a)-min(a)) 
return(s)
}

wyszukaj<-function(v,x){
positions <- which(v > x)
s <- length(positions)
return(s)
}