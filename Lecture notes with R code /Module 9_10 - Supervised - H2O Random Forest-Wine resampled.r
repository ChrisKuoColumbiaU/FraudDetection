
# Get the "Wine" dataset
#install.packages("rattle.data")
library(rattle.data)
head(wine)
table(wine$Type)

# Take only Type='1'
temp <-wine[wine$Type=='1',]
# Generate 1,000 row samples with replacement. Note if replace=False, it won't work.
temp2<-temp[sample(nrow(temp), 4000,replace=T), ]
dim(temp2)
# Put together
wine2<-rbind(wine[wine$Type=='2',],temp2)
# Shuffle the rows
wine2 <- wine2[sample(nrow(wine2)),]
dim(wine2)
# Change the target value to 1 and 0.
wine2$Type <-as.factor(ifelse(wine2$Type=='1','0','1'))
head(wine2)
#mean(as.numeric(wine2$Type))
table(wine2$Type)

df2 <-wine2[sample(nrow(wine2), 800), ]
df<-rbind(df2,wine2[wine2$Type=='1',])
#mean(as.numeric(df$Type))
table(df$Type)

#install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
library(dplyr)
library(h2o)
h2o.init(nthreads=-1)

# Convert to h2o dataframe
df.hex <-as.h2o(df)

splits <- h2o.splitFrame(
  data = df.hex, 
  ratios = c(0.2,0.2),   # the ratios should sum up to to be less than 1.0. 
    destination_frames = c("train", "valid", "test"), seed = 1234
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]
valid

## Exclude the variable 'Type'
predictors <- setdiff(names(df), 'Type')
predictors

rf_model <- h2o.randomForest(        
      training_frame = train,       
      validation_frame = valid,     
      x=predictors,                       
      y='Type',                         
      model_id = "rf_model",      
      ntrees = 20, #2000 is recommended                
      max_depth = 3, #30 is recommended               
      stopping_rounds = 2,          
      stopping_tolerance = 1e-2,    
      score_each_iteration = T,     
      seed=1234)                 

## Get the AUC on the validation set
h2o.auc(h2o.performance(rf_model, newdata = test)) 

Rf_predictions<-h2o.predict(object = rf_model,newdata = valid)


hyper_params = list( ntrees = seq(100,1000,200), 
                    max_depth=seq(2,12,3)   )

grid <- h2o.grid(
  hyper_params = hyper_params,
  
  search_criteria = list(strategy = "Cartesian"),
  
  algorithm="randomForest",
  
  grid_id="rf_grid",
  
  # Below are is the same as h2o.gbm()
  x = predictors, 
  y = 'Type', 
  training_frame = train, 
  validation_frame = valid,
  seed = 1234,                                                             
  stopping_rounds = 5,
  stopping_tolerance = 1e-8,
  stopping_metric = "AUC", 
  score_tree_interval = 10       
)

grid        

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("rf_grid", sort_by="auc", decreasing = TRUE)    
print(sortedGrid)

for (i in 1:10) {
  topModels <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.auc(h2o.performance(topModels, valid = TRUE)))
}

best_model <- h2o.getModel(sortedGrid@model_ids[[1]])
summary(best_model)

scoring_history <- as.data.frame(best_model@model$scoring_history)
#plot(scoring_history$number_of_trees, scoring_history$training_MSE, type="p") #training mse
#points(scoring_history$number_of_trees, scoring_history$validation_MSE, type="l") #validation mse

## get the actual number of trees
ntrees <- best_model@model$model_summary$number_of_trees

# All done. Shut down H2O.
h2o.shutdown(prompt=FALSE)
