
install.packages("fpc")
install.packages("factoextra")
install.packages("rattle.data")
install.packages("dbscan")
library(dbscan)
library(fpc)
library(factoextra)
library(rattle.data)

data("multishapes")
head(multishapes)
table(multishapes$shape)

df <- multishapes[, 1:2]

db <- fpc::dbscan(df, eps = 0.15, MinPts = 5) 
    # You are encouraged to test other MinPts 1, 5, 50, 100
    # eps: you are encouraged to test other values 0.01, 0.15, 0.5, 0.99, 2.0
table(db$cluster,multishapes$shape)

# Plot DBSCAN results
plot(db,df, main = "Multi-shapes identified by DBSCAN", frame = FALSE)

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
