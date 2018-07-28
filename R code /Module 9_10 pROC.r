
library(caret)
library(pROC)
head(iris)
table(iris$Species)

df <- iris[iris$Species!='setosa',]
head(df)
dim(df)

model <- train(df[,1:4], factor(df[,5]), 
               method = 'lda', 
               metric = 'ROC', 
               trControl = trainControl(method = 'repeatedcv', 
                                        number = 10, 
                                        repeats = 20, 
                                        savePredictions = T, 
                                        summaryFunction = twoClassSummary, 
                                        classProbs = T))

model

myRoc <- roc(response = model$pred$obs, predictor = model$pred$versicolor, positive = 'versicolor')
myRoc[]

# Print out some samples
data.frame(myRoc$sensitivities, myRoc$specificities, myRoc$thresholds)[1000:1010,]

plot(myRoc)
