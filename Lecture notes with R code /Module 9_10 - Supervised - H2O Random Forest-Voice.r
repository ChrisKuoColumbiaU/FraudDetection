
#install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
library(dplyr)
library(h2o)
h2o.init(nthreads=-1)

df.hex <- h2o.importFile(path = "/Users/chriskuo/Downloads/voice.csv")
head(df.hex)
h2o.table(df.hex$label)

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
predictors <- setdiff(names(df.hex), 'label')
predictors

rf_model <- h2o.randomForest(        
      training_frame = train,       
      validation_frame = valid,     
      x=predictors,                       
      y='label',                         
      model_id = "rf_model",      
      ntrees = 200, #2000 is recommended                
      max_depth = 10, #30 is recommended               
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
  y = 'label', 
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

# Calculate performance measures at threshold that maximizes precision
my.pred = h2o.predict(best_model,test)
head(my.pred)
my.perf = h2o.performance(best_model, test)
my.perf 

tpr=as.data.frame(h2o.tpr(my.perf))
fpr=as.data.frame(h2o.fpr(my.perf))
ROC_out<-merge(tpr,fpr,by='threshold')
head(ROC_out)

# Then give the pdf output file a name 
pdf(file="/Users/chriskuo/Downloads/my_ROC.pdf")
ggplot(ROC_out, aes(x = fpr, y = tpr)) +
  theme_bw() +
  geom_line() +
  ggtitle("ROC")
dev.off()

h2o.F1(my.perf)
precision=as.data.frame(h2o.precision(my.perf))
recall=as.data.frame(h2o.recall(my.perf))
PR_out<-merge(precision,recall,by='threshold')
head(PR_out)

# Then give the pdf output file a name 
pdf(file="/Users/chriskuo/Downloads/my_PR.pdf")
ggplot(out, aes(x = tpr, y = precision)) +
  theme_bw() +
  geom_line() +
  ggtitle("Precision-Recall")
dev.off()

# All done. Shut down H2O.
h2o.shutdown(prompt=FALSE)
