
#install.packages("h2o")
library(h2o)
library(dplyr)
library(ggplot2)
h2o.init()

wine <-read.csv("/Users/chriskuo/Downloads/wineQualityReds.csv")
head(wine)
dim(wine)
# convert data to H2OFrame
colnames(wine)
x_col <- setdiff(colnames(wine), c('X','quality'))
wine_h <- as.h2o(wine)
head(wine_h)

wine_train_test <- h2o.splitFrame(wine_h, 
                         ratios = c(0.4, 0.4), # must add up to less than 1.0
                         seed = 42)

train  <- wine_train_test[[1]]
test1  <- wine_train_test[[2]]
test2  <- wine_train_test[[3]]
dim(train)
dim(test1)
dim(test2)

model <- h2o.deeplearning(x = x_col,
                             training_frame = train,
                             model_id = "model",
                             autoencoder = TRUE,
                             hidden = c(5, 2, 5), 
                             epochs = 100,
                             activation = "Tanh")

test1_col <- h2o.deepfeatures(model, test1, layer = 2) %>% as.data.frame() 
quality <- as.vector(test1[,'quality'])
test1_col  <- cbind(test1_col,quality)
head(test1_col)
test1_col$quality <-as.character(test1_col$quality)
str(test1_col)

ggplot(test1_col, aes(x = DF.L2.C1, y = DF.L2.C2, color = quality)) +
  geom_point()
