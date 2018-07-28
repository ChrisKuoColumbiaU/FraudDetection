
library(e1071)
library(caret)

head(iris,5)
table(iris$Species)

iris$SpeciesClass[iris$Species=="versicolor"] <- "TRUE"
iris$SpeciesClass[iris$Species!="versicolor"] <- "FALSE"
trainPositive<-subset(iris,SpeciesClass=="TRUE")
testnegative<-subset(iris,SpeciesClass=="FALSE")
inTrain<-createDataPartition(1:nrow(trainPositive),p=0.6,list=FALSE)
trainpredictors<-trainPositive[inTrain,1:4]
trainLabels<-trainPositive[inTrain,6]

testPositive<-trainPositive[-inTrain,]
testPosNeg<-rbind(testPositive,testnegative)
table(testPosNeg$SpeciesClass)

testpredictors<-testPosNeg[,1:4]
testLabels<-testPosNeg[,6]

svm.model<-svm(trainpredictors,y=NULL,
               type='one-classification',
               nu=0.02,
               scale=TRUE,
               kernel="radial")
svm.predtest<-predict(svm.model,testpredictors)

confTest<-table(Predicted=svm.predtest,Reference=testLabels)
confusionMatrix(confTest,positive='TRUE')
