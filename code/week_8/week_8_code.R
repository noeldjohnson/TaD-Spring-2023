# Week 8: week 8 code for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 3-20-2022
# Last Updated: 3-27-2023
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez


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
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_8_code")

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

dim(news_dfm)

ids_train <- createDataPartition(1:nrow(news_dfm),
                                 p = 0.8, list = FALSE)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

#----------------------------------------
# 3. Using RandomForest                  ---
#----------------------------------------
mtry <- sqrt(ncol(train_x))  # number of features to sample at each split
# see: https://stats.stackexchange.com/questions/324370/references-on-number
# -of-features-to-use-in-random-forest-regression for more detail
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
metric <- "Accuracy" #how many labels the model got right out of the total 
# number of predictions. The percent of predictions that were correct
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

################
# Unsupervised Learning: Principal Components Analysis
################

# Set up workspace
rm(list = ls())

# Loading packages
#install.packages("lsa")
#install.packages("factoextra")

library(quanteda)
library(quanteda.corpora)
library(tidyverse)

library(factoextra) # makes it easy to work with PCA (great for visualization)
library(text2vec)
library(lsa)

#set directory
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_8_code")

###########
## 1 PCA ##
###########

# 1.1 Two functions in base R for PCA:
# see: http://www.sthda.com/english/articles/31-principal-component-methods
#-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp
#/?prcomp # uses the singular value decomposition approach: 
#examines the covariances/correlations between individuals
#?princomp # uses the spectral decomposition (eigendecomposition) approach:
#examines the covariances/correlations between variables (need more 
#individuals than variables)

# Remember to center your data! (default = TRUE) -- use scale() on 
# your matrix beforehand, or the option in prcomp()
# And don't have any missing values!


# 1.2 Example
data("data_corpus_sotu")
SOTU <- corpus_subset(data_corpus_sotu, Date > "1900-01-01")

SOTU_dfm <- dfm(SOTU, 
                stem = T, 
                remove_punct = T, 
                remove = stopwords("english")
)


SOTU_mat <- convert(SOTU_dfm, to = "matrix") # convert to matrix

# run pca
SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)

# visualize eigenvalues (scree plot: shows percentage of variance 
# explained by each dimension)
fviz_eig(SOTU_pca, addlabels = TRUE)
#how many relevant dimensions are there?

# Loadings for each variable: columns contain the eigenvectors
SOTU_pca$rotation[1:10, 1:5]
dim(SOTU_pca$rotation)

# Can we interpret the dimensions?
pc_loadings <- SOTU_pca$rotation
# what do we expect this correlation to be?
cor(pc_loadings[,1], pc_loadings[,2])  # these should be orthogonal
# top loadings on PC1
# token loadings
N <- 10
pc1_loading <- tibble(token = rownames(pc_loadings), 
                      loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
#lets get the top and bottom 10
pc1_loading <- rbind(top_n(pc1_loading, N, loading),
                     top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token,
                                                     levels = unique(token)))

# plot top tokens according to absolute loading values
ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity",
           fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens with Top Loadings on PC1") +
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank())

# Value of the rotated data: your "new", dimensionality reduced data
View(SOTU_pca$x)  # each observation 

# retrieve most similar documents

# function computes cosine similarity between query and all
# documents and returns N most similar
# query is always the nearest neighbor hence dropped
nearest_neighbors <- function(query, low_dim_space, N = 5, norm = "l2"){
  cos_sim <- sim2(x = low_dim_space, y = low_dim_space[query, , drop = FALSE],
                  method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])
}

# apply to document retrieval
nearest_neighbors(query = "Obama-2009",
                  low_dim_space = SOTU_pca$x, N = 5, norm = "l2")

# Visualization resources:

# Tutorial from factoextra author about how to use his package to
#explore and visualize PCA results: 
#http://www.sthda.com/english/articles/
#31-principal-component-methods-in-r-practical-guide/
#112-pca-principal-component-analysis-essentials/
# See here for visualizing PCA with the ggbiplot library:
#https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/



fviz_pca_ind(SOTU_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             label = "none"
)

fviz_pca_ind(SOTU_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)




# end code



