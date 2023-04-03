# Week 9 code for TaD: clustering 
# Author: Noel Johnson (adapted from many others)
# Created: 4-18-2022
# Last Updated: 4-3-2023

# Cluster Analysis

# install.packages("factoextra")

library(tidyverse)
library(factoextra)

# The built-in R dataset USArrests is used

# Load and scale the dataset
data("USArrests")
df <- scale(USArrests)
head(df)

# Distance matrix computation and visualization
# Correlation-based distance method
res.dist <- get_dist(df, method = "pearson")
head(round(as.matrix(res.dist), 2))[, 1:6]
# Visualize the dissimilarity matrix
fviz_dist(res.dist, lab_size = 8)

# k-means clustering
res.km <- eclust(df, "kmeans", k = 4)

# Print result
res.km

# optimal number of clusters? Elbow Method
fviz_nbclust(df, kmeans, method = "wss")

# Hiearchical clustering analysis
# Load and scale the dataset
data("USArrests")
df <- scale(USArrests)

# Compute dissimilarity matrix
res.dist <- dist(df, method = "euclidean")

# Compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")

# Visualize
plot(res.hc, cex = 0.5)

#

















# end code