# Week 2 introducing_quanteda for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 1-30-2022
# Last Updated: 2-3-2022
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia.

#-----------------------------
# SETTING UP
#-----------------------------

## Set up workspace
# Clear Global Environment
rm(list = ls())
# allow for exact replication
set.seed(100)

## Install quanteda
# Install the latest stable version of quanteda from CRAN
# run this if you don't have quanteda already installed
# install.packages("quanteda")

library(quanteda)
library(tidyverse)

# Devtools and the quanteda corpus
# Install the package "devtools" which is used to install packages 
# directly from Github
# install.packages("devtools")
library("devtools")

# Use devtools to install some sample data
devtools::install_github("quanteda/quanteda.corpora")

# Load it into our environment
library(quanteda.corpora)

# Read about the data available: https://github.com/quanteda/quanteda.corpora
# Note: Like many R packages, quanteda is still under development so it is 
#always changing! 
# New features are being added but sometimes functions or function 
# parameters are deprecated or renamed.
# to check version
packageVersion("quanteda")

# How would you get an older version of quanteda? 
# (For example, if you accidentally installed the dev version from 
# GitHub but you want to go back to the last stable release, or you 
# want a legacy version to support old code.)

# - Check the CRAN archive
# use the install_version function, e.g.:
# devtools::install_version("quanteda", version = "0.99.12", 
# repos = "http://cran.us.r-project.org")

# If you want the latest dev version of quanteda, it's on GitHub, 
# devtools::install_github("quanteda/quanteda") 
# But we will use the latest version from CRAN for stability/sanity reasons
 
# Concept review:
# token
# type
# feature
# term

#-----------------------------
# THE CORPUS OBJECT
#-----------------------------

# quanteda's main input object is called a "corpus" 
# (a way of organizing text data: generally includes text + metadata)

# THERE ARE OTHER WAYS to organize text data
# TAKE A LOOK AT: https://www.tidytextmining.com/tidytext.html

# load the State of the Union (SOTU) corpus
sotu <- data_corpus_sotu

# a corpus consists of:
# (1) documents: text + doc level data 
# (2) corpus metadata
# (3) extras (settings)
head(docvars(sotu))  # document-level variables
meta(sotu)  # corpus-level meta-data

# ndoc identifies the number of documents in a corpus
ndoc(sotu)
# save it as an object to be used later
ndocs <- ndoc(sotu)

# summary of the corpus (provides some summary statistics on the 
# text combined with the metadata)
corpusinfo <- summary(sotu, n = ndocs)  # note n default is 100
head(corpusinfo)
tail(corpusinfo)

# does tokens >= types always hold?

# quick visualization
token_plot <- ggplot(data = corpusinfo, aes(x = Date, y = Tokens)) +
  geom_line() + geom_point() + theme_bw()
token_plot

# subset corpus  #Biden is not yet in the corpus
summary(corpus_subset(sotu, President == "Trump"))
trump_sotu <- corpus_subset(sotu, President == "Trump")
trump_sotu


# key words in context (KWIC)
kwic_america <- kwic(trump_sotu, pattern = "america",
                     valuetype = "regex", window = 6)
head(kwic_america)

# keep only the text of the the 2018 SOTU
trump_2018_text <- as.character(trump_sotu)[2]
trump_2018_text

#-----------------------------
# TOKENIZING & STEMMING
#-----------------------------

# Running a basic text analysis

# start with a short character vector
sampletxt <- "The police with their policing strategy instituted a policy? 
of general iterations at the Data Science Institute."

# Let's tokenize (break vector into individual words)
tokens <- tokens(sampletxt)
tokens
tokens <- tokens(sampletxt, remove_punct = TRUE)
tokens
names(tokens)
tokens[["text1"]]

# Stemming examples # SnowballC stemmer is based on the Porter stemmer 
# (varies by language, english is default)
stems <- tokens_wordstem(tokens)
names(stems)
stems[["text1"]]

# Tokenizing text
?tokens
tokenized_speech <- tokens(trump_2018_text)
head(unname(unlist(tokenized_speech)), 20)

test <- unlist(tokenized_speech)
test
test <- unname(unlist(tokenized_speech))
test

# alternative using only base R
tokenized_speech <- strsplit(trump_2018_text, " ")

# remove punctuation when tokenizing
tokenized_speech <- tokens(trump_2018_text, remove_punct = TRUE)
head(unname(unlist(tokenized_speech)), 20)

# Stemming ---------------------
?tokens_wordstem
stemmed_speech <- tokens_wordstem(tokenized_speech)  
head(unname(unlist(stemmed_speech)), 20)

# Ngrams ---------------------
tokenized_speech_ngrams <- tokens(trump_2018_text, remove_punct = TRUE)
tokenized_speech_ngrams <- tokens_ngrams(tokenized_speech_ngrams,
                                         n = 2L)

head(unname(unlist(tokenized_speech_ngrams)), 20)
tail(unname(unlist(tokenized_speech_ngrams)), 20)

## Types vs. Tokens
ntoken(trump_2018_text)
ntype(trump_2018_text)
tokens(trump_2018_text) %>%
  unlist() %>%
  unique() %>%
  length()

#-----------------------------
# DOCUMENT FEATURE MATRIX (~ DTM)
#-----------------------------

# Creating a DFM
#Arguments passed in the token function can be passed into the DFM
# input can be a document, corpus, etc 
trump_2018_dfm <- dfm(trump_2018_text)  

# inspect the first few features
trump_2018_dfm[ , 1:10]

# how many rows does this dfm have? 
dim(trump_2018_dfm)

# top features in dfm
topfeatures(trump_2018_dfm)

# Are all of these features relevant?
# Stopwords?
# Punctuation (maybe!!! --> think what the goal is. Can theory help?)

# if you want, can convert to a data frame
out <- convert(dfm(trump_2018_dfm), to = "data.frame")


#-----------------------------
# PREPROCESSING (~FEATURE ENGINEERING)
#-----------------------------
# pre-processing can be done prior to dfm OR use the pre-processing 
# arguments of dfm
?dfm  # to see all options
# NOTE: lowercase argument is by default TRUE
# punctuation
trump_2018_dfm <- dfm(trump_2018_text, remove_punct = TRUE)
trump_2018_dfm[, 1:10]

# stemming
trump_2018_dfm <- dfm(trump_2018_text, stem = TRUE, remove_punct = TRUE)
trump_2018_dfm[, 1:10]
# can also apply stemmer afterwards using dfm_wordstem() on a dfm object

# Stopwords ---------------------
# stopwords are common words that (presumably) add little understanding 
# to the content of the document by themselves
# The stopwords function takes a language as an input and produces a 
# vector of stopwords compiled from that language

stopwords("chinese") # from Snowball

# Read about languages/sources in the stopwords package
# https://cran.r-project.org/web/packages/stopwords/index.html
# Fun fact: Quanteda also supports stopwords for english, 
# danish, french, greek, hungarian, 
# norwegian, russian, swedish, catalan, dutch, finnish, german, 
# italian, portuguese, spanish, and arabic

trump_2018_dfm_1 <- dfm(trump_2018_text, remove_punct = TRUE)
trump_2018_dfm_2 <- dfm(trump_2018_text, remove = stopwords("english"),
                        remove_punct = TRUE)

topfeatures(trump_2018_dfm_1)
topfeatures(trump_2018_dfm_2)

# install.packages("quanteda.textplots")
library(quanteda.textplots)

# wordclouds
textplot_wordcloud(trump_2018_dfm_1, max_words = 100)
textplot_wordcloud(trump_2018_dfm_2, max_words = 100)


#-----------------------------
# WEIGHTED DOCUMENT FEATURE MATRIX
#-----------------------------

# WHAT ARE WE WEIGHTING?
# Now we will create a DFM of all the SOTU speeches
full_dfm <- dfm(sotu, remove = stopwords("english"), remove_punct = TRUE)
full_dfm[, 1:5]  # notice sparsity
dim(full_dfm)
topfeatures(full_dfm) # all speeches
topfeatures(full_dfm[nrow(full_dfm),]) # last speech

# tfidf: term frequency-inverse document frequency
# uses the absolute frequency of terms in each document
?dfm_tfidf
weighted_dfm <- dfm_tfidf(full_dfm)
topfeatures(weighted_dfm)
topfeatures(weighted_dfm[nrow(weighted_dfm),])

# tfidf - Relative frequency weighting
?dfm_weight
proportional <- dfm_weight(full_dfm, scheme = "prop")
topfeatures(proportional)
topfeatures(proportional[nrow(proportional),])
# This is a composition of proportional term frequency weighting and inverse 
# document weighting. Proportional term frequency is feature proportions 
# within documents: rows of DFM sum to 1.
normalized <- dfm_tfidf(full_dfm, scheme_tf = "prop") 
topfeatures(normalized)
topfeatures(normalized[nrow(normalized),])

#-----------------------------
# COLLOCATIONS
#-----------------------------

# install.packages("quanteda.textstats")
library(quanteda.textstats)

#to identify common multi-wordy expressions
# bigrams
head(textstat_collocations(trump_2018_text)) # see documentation

# trigrams
?textstat_collocations
head(textstat_collocations(trump_2018_text, size = 3))

#ngrams up to trigrams
textstat_collocations(trump_2018_text, size=2:3)

####################################
# code to convert from dfm or corpus object into data frame
####################################
test <- convert(full_dfm, to = "data.frame")
test_tbl <- as_tibble(test)
head(test_tbl)
names(test_tbl)
####################################

#-----------------------------
## TOKENIZING NON LATIN LANGUAGES
# ---------------------

# Chinese example taken from:
# https://quanteda.io/articles/pkgdown/examples/chinese.html

# Japanese example here:
# https://tutorials.quanteda.io/language-specific/japanese/

# Tokenizing Chinese text

corp <- 
  quanteda.corpora::download(url = "https://www.dropbox.com/s/37ojd5knz1qeyul/
                             data_corpus_chinesegovreport.rds?dl=1")

# Chinese stopwords
ch_stop <- stopwords("zh", source = "misc")
ch_stop

# tokenize
ch_toks <- corp %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = ch_stop)

# construct a dfm
ch_dfm <- dfm(ch_toks)
topfeatures(ch_dfm)

# load fonts for wordcloud
extrafont::font_import(pattern = "SimHei")

textplot_wordcloud(ch_dfm, min_count = 500, random_order = FALSE,
                   rotation = .25, max_words = 100,
                   min_size = 0.5, max_size = 2.8,
                   font = if (Sys.info()['sysname'] == "Darwin") "SimHei" else NULL,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))


# Tokenizing Japanese text -----------------------
#corp <- quanteda.corpora::download("data_corpus_foreignaffairscommittee")

# use the latest 1000 speeches
#txt <- tail(texts(corp), 1000)
#
## Dictionary based boundary detection
#toks_icu <- tokens(txt)
#head(toks_icu[[20]], 100)
#
#ja_dfm <- dfm(toks_icu, remove = stopwords("ja", source = "stopwords-iso"))
#topfeatures(ja_dfm)

#-----------------------------
# REGULAR EXPRESSIONS
#-----------------------------
# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of
# cheatsheet for regex:
# https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

# grep
s_index <- grep(" s ", texts(sotu))
head(s_index)

# grepl
s_index <- grepl(" s ", texts(sotu))
table(s_index)

# grepl
thank_index <- grepl("^Thank", texts(sotu))
table(thank_index)

# this returns every speech that contains " s " -- JUST THE LETTER S BY ITSELF
texts_with_s <- grep(" s ", texts(sotu), value = TRUE)

# Here we create a vector of documents with " s " removed
texts_without_s <- gsub(" s ", "",  sotu)

# ALWAYS TEST FIRST
gsub(" s ", " ",  "hello how s are you")
grepl("^so", c("so today we", "never so today", "today never so"))

# SUGGESTED PACKAGE to deal with regular expressions: stringr


# Questions?

# Resources

# https://quanteda.io/articles/quickstart.html

# https://tutorials.quanteda.io/



