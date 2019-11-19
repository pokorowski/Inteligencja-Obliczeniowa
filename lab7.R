library("dbscan")
require(graphics)
iris <- read.csv("iris2D.csv",colClasses=c("NULL",NA,NA))
km.res <- kmeans(iris, 3, nstart = 25)
#plot(x = iris$PC1, y = iris$PC2, col = km.res$cluster)

res.db <- dbscan::dbscan(iris, 0.5, 3)
plot(x = iris$PC1, y = iris$PC2, col = res.db$cluster)
