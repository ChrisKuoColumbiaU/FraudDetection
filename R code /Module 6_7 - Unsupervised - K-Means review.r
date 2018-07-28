
#install.packages("rattle.data")
library(datasets)
library(rattle.data)

head(iris)
table(iris$Species)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20) #This means that R will try 20 different random starting assignments and then select the one with the lowest within cluster variation.
irisCluster

iris_PCA <- prcomp(iris[,1:4])
head(iris_PCA)
iris_PCA2 <-iris_PCA$x[,1:2]

# Plot DBSCAN results
plot(iris_PCA2, main = "iris_PCA identified by K-means", frame = FALSE)

head(wine)
table(wine$Type)

wn <-wine[,-1]
head(wn)

wine_PCA <- prcomp(wn)
head(wine_PCA)
wine_PCA2 <-wine_PCA$x[,1:2]

# Compute DBSCAN using fpc package
set.seed(1)
modl <- fpc::dbscan(wn, eps = 50, MinPts = 10)
plot(wine_PCA2, main = "Wine Type identified by DBSCAN", type="p", col=modl$cluster, frame = FALSE)

table(wine$Type,modl$cluster)
