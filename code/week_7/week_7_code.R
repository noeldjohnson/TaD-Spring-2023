# Week 7: week 7 code for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 3-21-2022
# Last Updated: 3-20-2023
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())
set.seed(1234)

# load required libraries
library(tidyverse)
library(caret)
# EXCELLENT DOCUMENTATION https://topepo.github.io/caret/index.html
library(quanteda)

# set working directory
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_7_code")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
table(news_data$category)

# let's work with 2 categories
news_samp <- news_data %>%
  filter(category %in% c("WEIRD NEWS", "WORLD NEWS")) %>%
  select(headline, category) %>%
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "WORLD NEWS"])
head(news_samp$text[news_samp$class == "WEIRD NEWS"])

# some pre-processing (the rest will let dfm do)
# replace apostrophes
news_samp$text <- gsub(pattern = "'", "", news_samp$text)
news_samp$class <- recode(news_samp$class,  "WEIRD NEWS" = "weird",
                          "WORLD NEWS" = "world")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL # not sure why we need this (NDJ)...

#----------------------------------------
# 2. Support Vector Machine (SVM) using Caret ---
#----------------------------------------

# create document feature matrix
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE,
                remove = stopwords("english")) %>% convert("matrix")

# A. the caret package has its own partitioning function
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE,
                                 times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options
# none: only fits one model to the entire training set
# i.e. no cross-validation
trctrl <- trainControl(method = "none")


# C. train model (caret gives us access to even more options)
# see: https://topepo.github.io/caret/available-models.html

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
svm_linear_cmat

# How to see the coefficients
coefs <- svm_mod_linear$finalModel@coef[[1]]
mat <- svm_mod_linear$finalModel@xmatrix[[1]]
test <- coefs %*% mat

# svm - radial
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = test_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, test_y)
svm_radial_cmat

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)
# Accuracy: Overall, how often is the classifier correct?
# [(Pred "Yes" & Actually "Yes") + (Pred "No" & Actually "No")]/Total

#----------------------------------------
# example with cross-validation
#----------------------------------------
# https://topepo.github.io/caret/model-training-and-tuning.html

# now we will have train / test / validation
val_x <- test_x
val_y <- test_y 
trctrl <- trainControl(method = "cv",
                       number = 5)

# Also available: Leave One Out CV
#trctrl <- trainControl(method = "LOOCV", p = 0.8)

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

# predict on heldout validation data
svm_linear_pred <- predict(svm_mod_linear, newdata = val_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, val_y)

# confusion matrix on predictions
svm_linear_cmat

#----------------------------------------

trctrl <- trainControl(method = "cv",
                       number = 3)

# svm - radial #this takes a long time to run!
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = val_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, val_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)

#----------------------------------------
# Random Forests                     ---
#----------------------------------------

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())

set.seed(1234)

# load required libraries
library(tidyverse)
library(randomForest)
library(mlbench)
library(caret)

# set working directory
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_7_code")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------

# same data as last week, but we will look at different categories
news_data <- readRDS("news_data.rds") 
table(news_data$category)

# let's work with 2 categories and get a sample of 500 news
# remember the order of operations matters! We first select category,
# group by, and then sample 500 obs
news_samp <- news_data %>% 
  filter(category %in% c("MONEY", "LATINO VOICES")) %>% 
  group_by(category) %>%
  # sample 500 of each to reduce computation time (for lab purposes)
  sample_n(500) %>%  
  ungroup() %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "MONEY"])
head(news_samp$text[news_samp$class == "LATINO VOICES"])

# some pre-processing (the rest we'll let dfm do)
# replace apostrophes
news_samp$text <- gsub(pattern = "'", "", news_samp$text)
news_samp$class <- recode(news_samp$class,
                          "MONEY" = "money", "LATINO VOICES" = "latino")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL

#----------------------------------------
# 2. Prepare Data                        ---
#----------------------------------------
library(quanteda)

# create document feature matrix, actually a MATRIX object this time!
# keep tokens that appear in at least 5 headlines
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE,
                remove = stopwords("english")) %>% 
  dfm_trim(min_termfreq = 5) %>% 
  convert("matrix")

ids_train <- createDataPartition(1:nrow(news_dfm),
                                 p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

#----------------------------------------
# 3. Using RandomForest                  ---
#----------------------------------------
mtry <- sqrt(ncol(train_x))  # number of features to sample at each split
ntree <- 51  # num of trees to grow
# more trees generally improve accuracy but at the cost of computation time
# odd numbers avoid ties (recall default aggregation is "majority voting")

system.time(rf.base <- randomForest(x = train_x, y = train_y, ntree = ntree,
                                    mtry = mtry, importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])

# print results
print(rf.base)

# plot importance
# gini impurity = how "pure" is given node ~ class distribution
# = 0 if all instances the node applies to are of the same class
# upper bound depends on number of instances
varImpPlot(rf.base, n.var = 10, main = "Variable Importance")

#?predict.randomForest

predict_test <- predict(rf.base, newdata = test_x)
confusionMatrix(data = predict_test, reference = test_y)

# to tune hyperparameters, use:
# ?tuneRF


#----------------------------------------
# 4. 5-Fold CV RandomForest Using Caret            ---
#----------------------------------------
# note that the RF model in caret calls randomForest, but it's wrapped in caret

trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
mtry <- sqrt(ncol(train_x))
ntree <- 51  
tunegrid <- expand.grid(.mtry = mtry)
system.time(rf.caret <- train(x = train_x, y = train_y, 
                              method = "rf", metric = metric, 
                              tuneGrid = tunegrid, trControl = trainControl,
                              ntree = ntree)
)

# print results
print(rf.caret)

rf_predict <- predict(rf.caret, newdata = test_x)
confusionMatrix(rf_predict, reference = test_y)

# plot importance
varImpPlot(rf.caret$finalModel, n.var = 10, main = "Variable Importance")

#----------------------------------------
# 5. RandomForest Using Caret + tuning   ---
#----------------------------------------
# we are going to gridsearch over 1 parameter: mtry

trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
# at the moment caret only allows tuning of mtry (partly b/c ntree is just
# a matter of computational constraints)
tunegrid <- expand.grid(.mtry = c(0.5*mtry, mtry, 1.5*mtry))  
system.time(rf.grid <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid,
                             trControl = trainControl, 
                             ntree = ntree)
)
# print grid search results
print(rf.grid)

# plot grid search results
plot(rf.grid)

#----------------------------------------
# 6. RandomForest Using Caret + manual tuning ---
#----------------------------------------
# we have one value for mtry and we will train 3 models with different 
# values for ntree

tunegrid <- expand.grid(.mtry = mtry)

# ntree = 1
system.time(rf.man1 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid,
                             trControl = trainControl, 
                             ntree = 1))

# ntree = 5
system.time(rf.man2 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid,
                             trControl = trainControl, 
                             ntree = 5))

# ntree = 51
system.time(rf.man3 <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid,
                             trControl = trainControl, 
                             ntree = 51))

# collect results & summarize
results <- resamples(list(rf1 = rf.man1, rf5 = rf.man2, rf51 = rf.man3))
summary(results)

# test set accuracy
(cm <- confusionMatrix(predict(rf.man1, newdata = test_x), test_y))
# access the components of the results with the $ operator
cm$table
cm$overall

confusionMatrix(predict(rf.man2, newdata = test_x), test_y)
confusionMatrix(predict(rf.man3, newdata = test_x), test_y)

# box and whisker plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

# reminder: Kappa = Cohen's Kappa, compares observed accuracy with
# expected accuracy (think: baseline accuracy)




# end code



# how is the CV best model selected?

# function used to select the optimal tuning parameter
trainControl$selectionFunction
?best
