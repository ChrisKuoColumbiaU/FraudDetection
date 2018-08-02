
#install.packages("MeanShift")
#install.packages("ggfortify")
library(MeanShift)
library(ggfortify)

df <- iris[c(1, 2, 3, 4)]
autoplot(prcomp(df), data = iris, colour = 'Species',frame = TRUE)

set.seed( 2 )
iris.sd<-scale(iris[,1:4])     # mean and sd
iris.data <-t(iris.sd)
head(iris.data)
dim(iris.data)

ms_clustering <- msClustering( iris.data, h=1.0 )
print( ms_clustering )
# options( mc.cores=2 )
# ms_clustering <- msClustering( iris.data, multi.core=TRUE )

iris_ms <- iris
iris_ms[,'ms_label'] <- ms_clustering[2]
iris_ms[,'ms_label'] <- as.factor(iris_ms[,'ms_label'])
autoplot(prcomp(iris_ms[,1:4]), data = iris_ms, colour = 'ms_label',frame=TRUE)

bms_clustering <- bmsClustering( iris.data,h=1.0)
print( bms_clustering )

iris_bms <- iris
iris_bms[,'bms_label'] <- bms_clustering[2]
iris_bms[,'bms_label'] <- as.factor(iris_bms[,'bms_label'])
autoplot(prcomp(iris_bms[,1:4]), data = iris_bms, colour = 'bms_label',frame=TRUE)
