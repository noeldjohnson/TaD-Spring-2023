# Week 3: descriptive inference for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 2-7-2022
# Last Updated: 2-5-2023
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia.

## Set up Quanteda 

# Clear Global Environment
rm(list = ls())

# devtools::install_github("quanteda/quanteda.corpora")

# Libraries
library(tidyverse)
library(quanteda)
library(quanteda.corpora)

#-----------------------------
# text encoding
#-----------------------------

# Text encoding

# What is text encoding?
# How do you figure out what kind you have 
# (e.g. scraped text from the Internet)?
# What kind of encoding can R and/or quanteda handle?

# Some types of text encoding
# character encoding is a set of mappings between the bytes in the 
# computer and the characters in the character set.
# UTF-8
# ASCII (subset of UTF-8)
# Latin-1

# UTF-8 represents characters from European languages 
# (English, Spanish, German, French, etc) and some characters from 
# Chinese/Japanese/Korean, plus emojis.

# Note: Text obtained from Internet sources can be messy. Issues can 
# especially arise when you are working with texts from multiple sources 
# and you end up with a mixture of encodings. This can cause the encoding 
# to be detected incorrectly when you read in the text.

# What encoding do you have?

# You can check with this function in base R
validUTF8("This is a sentence")

# You can use the package utf8 by Patrick Perry
# Read about it here: https://cran.r-project.org/web/packages/utf8/index.html
# install.packages("utf8")
library("utf8")

as_utf8("\xF0\x9F\x98\x8D")
print("\xF0\x9F\x98\x8D")
# emojis unicodes: https://apps.timwhitlock.info/emoji/tables/unicode

# What if you get a weird character and you're not sure?

# install.packages("stringi")
library("stringi")

# Use the encoding guesser to guess what this character is
stri_enc_detect("0x00E3")

# It's only a guess!

# What's ISO-8859-1?
# This is another name for the Latin-1 encoding. 

# How do you convert encodings?
test_str <- "São Paulo"
validUTF8(test_str)
converted_str <- iconv("São Paulo", from = "UTF-8", to = "latin1")

converted_str
validUTF8(converted_str)

#charToRaw converts a character string to raw bytes
charToRaw(converted_str) # Latin-1 encoding

charToRaw(test_str) # UTF-8 encoding

# But what about here?
iconv("ã", from = "UTF-8", to = "ASCII")

# In most cases, your text will probably already be in UTF-8. When you 
#import text and csv files you can specify the encoding.
# The authors of quanteda have also written a package called readtext() 
# that can also deal with encodings in text corpora!
# In most cases, you want to convert your text to UTF-8 (with the 
# possible exception of languages that do not use the Latin alphabet)


#-----------------------------
# HEAP'S LAW
#-----------------------------
# Token-type relationship in corpus

# Heaps law estimates vocabulary size as a function of collection size:
#     M = kT^b

# M = vocab size (num of types)
# T = number of tokens
# k, b are constants with typical values of:
# 30 <= k <= 100
# 0.4 <= b <= 0.6

# 2.1 Example using data from the corpus of inaugural speeches
tokens <- tokens(data_corpus_inaugural, remove_punct = TRUE) 
num_tokens <- sum(lengths(tokens))

inaug_dfm <- dfm(data_corpus_inaugural)

M <- nfeat(inaug_dfm)  # number of types

# Let's check using parameter values from MRS Ch.5 for a corpus with 
# more than 100,000 tokens

k <- 54
b <- 0.49

k * (num_tokens)^b

M

# Let's think about why (what types of texts are these?)
# The parameters are quite variables because vocabulary growth depends a 
# lot on the nature of the collection and how the text is processed

# Lets try to model the relationship again with New parameters
k <- 44
b <- 0.455

k * (num_tokens)^b

M

# You can solve mathematically for k and b or fit a model to find k and b 
# -- relationship between log(collection size) and log(vocab size) is linear

#-----------------------------
# ZIPF'S LAW
#-----------------------------
# Term frequency in corpus and rank

# x-axis: log of ranks 1 through 100
# y-axis log of frequency of top 100 terms from the DFM

plot(log10(1:100), log10(topfeatures(inaug_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Top 100 
     #Words in U.S. Presidential Inaugural Speech Corpus")

# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(inaug_dfm, 100)) ~ log10(1:100))
abline(regression, col = "red")

# Returns the 95% confidence intervals for the regression coefficients
# confint(regression)

# Provides R-squared, F-test, and cofficient estimates from regression
summary(regression)

## Stopwords: do they affect Zipf's law?

inaug_dfm_nostop <- dfm(data_corpus_inaugural, remove=stopwords("english"))

plot(log10(1:100), log10(topfeatures(inaug_dfm_nostop, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Top 100 Words 
     #in U.S. Presidential Inaugural Speech Corpus (w/o stopwords)")

# Regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(inaug_dfm_nostop, 100)) ~ log10(1:100))
abline(regression, col = "red")
confint(regression)
summary(regression)

#-----------------------------
# MEASURING SIMILARITY
#-----------------------------

# Cosine similarity--take the dot product of two vectors
# cos = x*y/|x||y|
calculate_cosine_similarity <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  
# %*% specifies dot product rather than entry by entry multiplication 
# (we could also do: sum(x * y))
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  return(nominator/denominator)
}

# example 1
x <- c(1, 2, 3)
y <- c(1, 2, 3)

# what should we get?
calculate_cosine_similarity(x, y)

# example 2
a <- c(1, 2, 3)
b <- c(1, 2, 40000)

calculate_cosine_similarity(a, b)

# Let's do it with texts
obama_text <- texts(corpus_subset(data_corpus_inaugural, 
                                  President == "Obama"))
lincoln_text <- texts(corpus_subset(data_corpus_inaugural, 
                                    President == "Lincoln"))

# Make a dfm of these two
obama_lincoln_dfm <- dfm(c(obama_text, lincoln_text), 
                         remove = stopwords("english"), stem = TRUE)

# Calculate similarity
library(quanteda.textstats)
similarity_obama_lincoln_with_preprocessing <- 
  textstat_simil(obama_lincoln_dfm, margin = "documents", method = "cosine")
as.matrix(similarity_obama_lincoln_with_preprocessing)

# Let's see how stopwords/stemming affect similarity

obama_lincoln_no_preprocessing <- dfm(c(obama_text, lincoln_text))

# Calculate similarity

similarity_obama_lincoln_with_no_preprocessing <- 
  textstat_simil(obama_lincoln_no_preprocessing, margin = "documents", 
                 method = "cosine")

as.matrix(similarity_obama_lincoln_with_no_preprocessing)

# Other options available: Manhattan distance, correlation, etc.
?textstat_simil

