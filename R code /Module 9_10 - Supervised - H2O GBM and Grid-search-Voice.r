
library(dplyr)
library(h2o)
h2o.init(nthreads=-1)

df <- h2o.importFile(path = "/Users/chriskuo/Downloads/voice.csv")
dim(df)
summary(df,exact_quantiles=TRUE)

# Specify the response variable
response <- "label"

# Make the response variable a categorical variable
df[[response]] <- as.factor(df[[response]])      

## Exclude the variable 'Type'
predictors <- setdiff(names(df), 'label')
predictors

splits <- h2o.splitFrame(
  data = df, 
  ratios = c(0.1,0.1),   # the ratios should sum up to to be less than 1.0. 
    destination_frames = c("train", "valid", "test"), seed = 1234
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

gbm <- h2o.gbm(x = predictors, y = response, training_frame = train)
gbm

## Get the AUC on the validation set
h2o.auc(h2o.performance(gbm, newdata = valid)) 

gbm <- h2o.gbm(x = predictors, y = response, 
               training_frame = train, 
               validation_frame = valid,
               learn_rate = .05, learn_rate_annealing =.99,
               ntrees=1000,
               stopping_rounds = 5,
               stopping_tolerance = 1e-4,
               stopping_metric = "AUC", 
               seed = 1234)

# print the auc for the validation data
print(h2o.auc(gbm, valid = TRUE))

hyper_params = list( ntrees = seq(100,3000,200), 
                    max_depth=seq(2,12,3)   )

grid <- h2o.grid(
  hyper_params = hyper_params,
  
  search_criteria = list(strategy = "Cartesian"),
  
  algorithm="gbm",
  
  grid_id="my_grid",
  
  # Below are is the same as h2o.gbm()
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  learn_rate = 0.05,                                                         
  learn_rate_annealing = 0.99,                                               
  sample_rate = 0.8,                                                       
  col_sample_rate = 0.8, 
  seed = 1234,                                                             
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC", 
  score_tree_interval = 10                                                
)

grid        

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("my_grid", sort_by="auc", decreasing = TRUE)    
print(sortedGrid)

for (i in 1:10) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  print(h2o.auc(h2o.performance(gbm, valid = TRUE)))
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
