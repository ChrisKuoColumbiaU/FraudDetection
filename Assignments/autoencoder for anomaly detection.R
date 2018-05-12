# https://shiring.github.io/machine_learning/2017/05/01/fraud

library(tidyverse)
library(dplyr)
library(ggplot2)

# download from https://www.kaggle.com/dalpozz/creditcardfraud
creditcard <- read.csv("/Users/chriskuo/Downloads/creditcard.csv")

The dataset contains numerical input variables V1 to V28, which are the result of a PCA transformation of the original features (which could not be provided due to confidentiality issues).

head(creditcard)
summary(creditcard$Time)

# This dataset is extremely imbalanced
table(creditcard$Class)

# separate transactions by day
creditcard$day <- ifelse(creditcard$Time > 3600 * 24, "day2", "day1")

# make transaction relative to day
creditcard$Time_day <- ifelse(creditcard$day == "day2", creditcard$Time - 86400, creditcard$Time)

summary(creditcard[creditcard$day == "day1", ]$Time_day)

summary(creditcard[creditcard$day == "day2", ]$Time_day)

# bin transactions according to time of day
creditcard$Time <- as.factor(ifelse(creditcard$Time_day <= 38138, "gr1", # mean 1st Qu.
                                    ifelse(creditcard$Time_day <= 52327, "gr2", # mean mean
                                           ifelse(creditcard$Time_day <= 69580, "gr3", # mean 3rd Qu
                                                  "gr4"))))

creditcard %>%
  ggplot(aes(x = day)) +
  geom_bar(color = "grey", fill = "lightgrey") +
  theme_bw()

creditcard <- select(creditcard, -Time_day, -day)

# convert class variable to factor
creditcard$Class <- factor(creditcard$Class)

creditcard %>%
  ggplot(aes(x = Time)) +
  geom_bar(color = "grey", fill = "lightgrey") +
  theme_bw() +
  facet_wrap( ~ Class, scales = "free", ncol = 2)

summary(creditcard[creditcard$Class == "0", ]$Amount)

summary(creditcard[creditcard$Class == "1", ]$Amount)

creditcard %>%
  ggplot(aes(x = Amount)) +
  geom_histogram(color = "grey", fill = "lightgrey", bins = 50) +
  theme_bw() +
  facet_wrap( ~ Class, scales = "free", ncol = 2)

## Modeling
install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)


# convert data to H2OFrame
creditcard_hf <- as.h2o(creditcard)


splits <- h2o.splitFrame(creditcard_hf, 
                         ratios = c(0.4, 0.4), 
                         seed = 42)

train_unsupervised  <- splits[[1]]
train_supervised  <- splits[[2]]
test <- splits[[3]]

response <- "Class"
features <- setdiff(colnames(train_unsupervised), response)

model_nn <- h2o.deeplearning(x = features,
                             training_frame = train_unsupervised,
                             model_id = "model_nn",
                             autoencoder = TRUE,
                             reproducible = TRUE, #slow - turn off for real problems
                             ignore_const_cols = FALSE,
                             seed = 42,
                             hidden = c(10, 2, 10), 
                             epochs = 100,
                             activation = "Tanh")

h2o.saveModel(model_nn, path="model_nn", force = TRUE)

model_nn <- h2o.loadModel("model_nn")
model_nn

#Convert to autoencoded representation
test_autoenc <- h2o.predict(model_nn, test)


# Anomaly detection
anomaly <- h2o.anomaly(model_nn, test) %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  mutate(Class = as.vector(test[, 31]))

mean_mse <- anomaly %>%
  group_by(Class) %>%
  summarise(mean = mean(Reconstruction.MSE))

ggplot(anomaly, aes(x = as.numeric(rowname), y = Reconstruction.MSE, color = as.factor(Class))) +
  geom_point(alpha = 0.3) +
  geom_hline(data = mean_mse, aes(yintercept = mean, color = Class)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "instance number",
       color = "Class")

anomaly <- anomaly %>%
  mutate(outlier = ifelse(Reconstruction.MSE > 0.02, "outlier", "no_outlier"))

anomaly %>%
  group_by(Class, outlier) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 