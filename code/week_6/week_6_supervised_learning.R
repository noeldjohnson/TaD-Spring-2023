# Week 6: week 6 code for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 3-7-2022
# Last Updated: 3-6-2023
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez

#----------------------------------------
# Some code for before main lecture   ---
#----------------------------------------

# illustrate floating point error for lecture slides
print(seq(0, 1, by=.1)[4], digits=2)
print(seq(0, 1, by=.1)[4], digits=2) == 0.3
print(seq(0, 1, by=.1)[4], digits=20)

#----------------------------------------
# Set up environment                  ---
#----------------------------------------
# clear global environment
rm(list = ls())

# set path where our data is stored
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_6_code")

# load required libraries
library(quanteda)
library(quanteda.corpora)
library(readtext)
library(tidyverse)
library(quanteda.textmodels)

#----------------------------------------
# Something I should have shared a while ago...  ---
#----------------------------------------

# https://quanteda.io/articles/pkgdown/quickstart.html
# quickstart.html#extracting-features-from-a-corpus

#----------------------------------------
# Supervised Learning: Naive Bayes    ---
#----------------------------------------
#source of data: https://www.kaggle.com/rmisra/
# news-category-dataset

# load data
news_data <- readRDS("news_data.rds")

# look at the data structure
glimpse(news_data)

# what are the categories in the category variable?
categories <- news_data %>% 
  filter(!is.na(category)) %>%
  count(category, sort = TRUE)

# subset data to only crime and sports and keep relevant variables text
# and class 
#(text=headline, class=category)
news_samp <- news_data %>%
  filter(category %in% c("CRIME", "SPORTS")) %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "CRIME"])
head(news_samp$text[news_samp$class == "SPORTS"])

# some pre-processing (the rest we will let dfm do)
# replace apostrophes
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  
head(news_samp$text[news_samp$class == "SPORTS"])

# what's the distribution of classes?
prop.table(table(news_samp$class))

# split sample into training & test sets
set.seed(1984L)
# we will use 80% of the data as our training set
prop_train <- 0.8 
ids <- 1:nrow(news_samp)
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)
ids_test <- ids[-ids_train]
train_set <- news_samp[ids_train,]
test_set <- news_samp[ids_test,]

# get dfm for each set
train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE,
                 remove = stopwords("english"))

# take a look at the dfm
train_dfm[, 1:10]

test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE,
                remove = stopwords("english"))

# take a look at the dfm
test_dfm[, 1:10]

# another way to take a look?
head(test_dfm)
head(train_dfm)

# Are the features of these two DFMs necessarily the same? Yes/No? Why?

# match test set dfm to train set dfm features
#?dfm_match
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))


# w/o smoothing ----------------

# train model on the training set
nb_model <- textmodel_nb(train_dfm, train_set$class,
                         smooth = 0, prior = "uniform")

# evaluate on test set
predicted_class <- predict(nb_model, newdata = test_dfm) 
predicted_class[1:10]
length(predicted_class)

# baseline
baseline_acc <- max(prop.table(table(test_set$class)))
baseline_acc

# get confusion matrix (see slide from class)
cmat <- table(test_set$class, predicted_class)
cmat
# accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_acc <- sum(diag(cmat))/sum(cmat)
# recall = TP / (TP + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) 
# precision = TP / (TP + FP)
nb_precision <- cmat[2,2]/sum(cmat[,2]) 
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)



# print using the concatenation command "cat"
cat(
  "\n",
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

# w smoothing ----------------

# train model on the training set using Laplace smoothing
nb_model_sm <- textmodel_nb(train_dfm, train_set$class,
                            smooth = 1, prior = "uniform")

# evaluate on test set
# don't know why we need "force=TRUE" here.... Works either way.
predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm, force=TRUE)

# get confusion matrix
cmat_sm <- table(test_set$class, predicted_class_sm)
# accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm)
# recall = TP / (TP + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) 
# precision = TP / (TP + FP)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) 
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

# print
cat(
  "\n",
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)

# take a look at the most discriminant features (do they pass the smell test)
posterior <- tibble(feature = rownames(t(nb_model_sm$param)), 
                    post_CRIME = t(nb_model_sm$param)[,1],
                    post_SPORTS = t(nb_model_sm$param)[,2])

# ?textmodel_nb
#$param (before $PcGw) is the class conditional posterior estimates

posterior %>% arrange(-post_SPORTS) %>% head(10)
posterior %>% arrange(-post_CRIME) %>% head(10)

# what does smoothing do? Reduces the "weight" placed on new 
# information (the likelihood) vis-a-vis the prior. 
plot(nb_model$param[1,], nb_model_sm$param[1,],
     xlim = c(0,0.02), ylim = c(0,0.02), xlab="No Smooth", ylab="Smooth") +
  abline(a = 0, b = 1, col = "red")

#----------------------------------------
# Classification using Word Scores    ---
#----------------------------------------

# Read in conservative and labour manifestos
filenames <- list.files(path = "cons_labour_manifestos")

# Party name and year are in the filename -- we can use regex to 
# extract these to use as our docvars
party <- unlist(regmatches(unlist(filenames),
                           gregexpr("^[[:alpha:]]{3}", unlist(filenames))))
year <- unlist(regmatches(unlist(filenames),
                          gregexpr("[[:digit:]]+", unlist(filenames))))

# Make a corpus with docvars from this data
cons_labour_manifestos <- corpus(readtext("cons_labour_manifestos/*.txt"))
docvars(cons_labour_manifestos, field = c("party", "year")) <- 
  data.frame(cbind(party, year))

# But we're going to use a dataframe (...actually a tibble)
cons_labour_df <- tibble(text = texts(cons_labour_manifestos),
                         class = party,
                         year = as.integer(year))

# Let's take a look...
glimpse(cons_labour_df)
colnames(cons_labour_df)

# what's the class distribution?
prop.table(table(cons_labour_df$class))

# randomly sample a test speech
 set.seed(1984L)
# set.seed(NULL)

ids <- 1:nrow(cons_labour_df)

#we are just excluding one for the test set
ids_test <- sample(ids, 1, replace = FALSE)
ids_train <- ids[-ids_test]
train_set <- cons_labour_df[ids_train,]
test_set <- cons_labour_df[ids_test,]

# create DFMs
train_dfm <- dfm(train_set$text, remove_punct = TRUE,
                 remove = stopwords("english"))
test_dfm <- dfm(test_set$text, remove_punct = TRUE,
                remove = stopwords("english"))

# Word Score model w/o smoothing ----------------
# Y variable must be coded on a binary x in {-1,1} scale, 
# so -1 = Conservative and 1 = Labour
# ?textmodel_wordscores
ws_base <- textmodel_wordscores(train_dfm, 
                          y = (2 * as.numeric(train_set$class == "Lab")) - 1 
)
# ==> Yea, so what the hell just happened with that last parameter?

train_set$class
train_set$class == "Lab"
as.numeric(train_set$class == "Lab")
(2 * as.numeric(train_set$class == "Lab"))
(2 * as.numeric(train_set$class == "Lab")) - 1

# Look at strongest features
# for labor
lab_features <- sort(ws_base$wordscores, decreasing = TRUE)  
lab_features[1:10]

# for conservative
con_features <- sort(ws_base$wordscores, decreasing = FALSE)  
con_features[1:10]

# Can also check the score for specific features
ws_base$wordscores[c("drugs", "minorities", "unemployment")]

# predict that test speech
test_set$class
predict(ws_base, newdata = test_dfm) 

# Word Score model w smoothing ----------------
# ?textmodel_wordscores
# Y variable must be coded on a binary x in {-1,1} scale, so 
# -1 = Conservative and 1 = Labour
ws_sm <- textmodel_wordscores(train_dfm, 
                        y = (2 * as.numeric(train_set$class == "Lab")) - 1, 
                        smooth = 1
)

# Look at strongest features
# for labor
lab_features_sm <- sort(ws_sm$wordscores, decreasing = TRUE)  
lab_features_sm[1:10]

# for conservative
con_features_sm <- sort(ws_sm$wordscores, decreasing = FALSE)  
con_features_sm[1:10]

# predict that last speech
test_set$class
predict(ws_base, newdata = test_dfm) 

# Smoothing  
plot(ws_base$wordscores, ws_sm$wordscores, xlim=c(-1, 1), ylim=c(-1, 1),
     xlab="No Smooth", ylab="Smooth")


#----------------------------------------
# Support Vector Machines                     ---
#----------------------------------------


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
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_6_code")

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

# some pre-processing (the rest we will let dfm do)
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





#










# end code