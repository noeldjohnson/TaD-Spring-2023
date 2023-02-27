# Week 5: week 5 code for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 3-1-2022
# Last Updated: 2-20-2023
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia.

#----------------------------------------
# Set up environment                   ---
#----------------------------------------

# set path where our data is stored
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_5_code")

# load required libraries
library(quanteda)
library(quanteda.corpora)
library(tidyverse)

#----------------------------------------
# Load data: conservative manifestos ---
#----------------------------------------
# read in the files
filenames <- list.files(path = "conservative_manifestos", full.names=TRUE)
filenames
cons_manifestos <- lapply(filenames, readLines)
# because readLines returns a vector with each elements = lines
cons_manifestos <- unlist(lapply(cons_manifestos,
                                 function(x) paste(x, collapse = " "))) 
View(cons_manifestos)

# get the date docvar from the filename
dates <- unlist(regmatches(unlist(filenames),
                           gregexpr("[[:digit:]]+", unlist(filenames))))

# construct tibble (a tibble is an "enhanced" data.frame)
# see ?tibble
manifestos_df <- tibble(year = dates, text = cons_manifestos)

#----------------------------------------
# Regular expressions                  ---
#----------------------------------------
# Let us take a step back and have a refresher on grep that we will
# need for later
words <- c("Washington Post", "NYT", "Wall Street Journal",
           "Peer-2-Peer", "Red State", "Cheese", "222", ",")

# Exploring by character type
#?grep
# Elements that have alphanumeric characters
grep("\\w", words, value = T)
# Elements that have words that are at least 7 characters long
grep("\\w{7}", words, value = T) 
# Elements that contain numbers
grep("\\d", words, value = T) 
# Elements that contain non-word characters (Including white space)
grep("\\W", words, value = T)  

# note that  grep returns the full element that matched the pattern
words2 <- c("voting", "votes", "devoted", "vote")

# Returns the index of matching items in the vector
grep("^vot", words2) 
# Returns the elements of the vector that matched the pattern
grep("^vot", words2, value = T) 
# Returns a logical vector indicating whether or not the component
# contains the expression
grepl("^vot", words2)  

# you can use the indices to select elements from the original vector
# that you want
words2[grepl("^vot", words2)]

presidents <- c("Roosevelt-33", "Roosevelt-37", "Obama-2003")

# Use gsub to replace patterns with a string
# Parentheses can identify components that can later be referenced by \\1 - \\2
gsub("(\\w+)-(\\d{2})", "\\1-19\\2", presidents) 
# We want to use the $ to indicate that the pattern should come at the end
# of the word, to avoid the mismatch in Obama-192003
gsub("(\\w+)-(\\d{2})$", "\\1-19\\2", presidents) 

# Note that regex expressions in R are similar to those in other
# languages but there are some key differences

# Resources:
# other packages to work with regular expressions: stringr, stringi
# cheatsheet for regex:
#https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# https://rstudio-pubs-static.s3.amazonaws.com/
# 74603_76cd14d5983f47408fdf0b323550b846.html
# http://r4ds.had.co.nz/strings.html#matching-patterns-with-regular-expressions

#----------------------------------------
# Selecting Features from DFM using Regular Expressions ---
#----------------------------------------

# Using simple texts

testText <- "The quick brown fox named Seamus jumps over the lazy dog also
named Seamus, with the newspaper from a a boy named Seamus, in his mouth."

# create a dfm
test1 <- dfm(testText)
# view as a dataframe
out <- convert(dfm(testText), to = "data.frame")

# keep only words ending in "s"
print(dfm(testText, select = "s$", valuetype = "regex")) 
# illustrate how you can access anything you want in the dfm
test2 <- dfm(testText, select = "s$", valuetype = "regex")
test2

testTweets <- c("2 + 2 = 4 #1984",
                "I thought you said the park? Why are we at the vet?
                #QuestionsFromPets",
                "Holy freeway #flooding Batman! #californiastorms taking
                their toll.")

# keep only hashtags i.e. expressions starting with a pound sign
print(dfm(testTweets, select="^#", valuetype = "regex"))  

# Selecting features from a corpus
data("data_corpus_irishbudgets")
irishbudgets_tokens <- tokens(data_corpus_irishbudgets)
irishbudgets_dfm <- dfm(irishbudgets_tokens) 
irishbudgtes_select <- dfm_select(irishbudgets_dfm, pattern=c("tax|budg|^auster"), 
                                  valuetype = "regex", selection="keep")
# valuetype = "regex" ensures that the select input will be interpreted
# as a regular expression

# You can pass a list of words to the "select" parameter in dfm,
# but using regular expressions can enable you to get all variants of a word
view(irishbudgtes_select[, 1:20])

dim(irishbudgtes_select)
topfeatures(irishbudgtes_select)

library(quanteda.textplots)

# wordclouds
textplot_wordcloud(irishbudgtes_select, max_words = 100)

#----------------------------------------
# Dictionaries                         ---
#----------------------------------------
# Here, dictionary = list of words


mytexts <- c("The new law included a capital gains tax,
             and an inheritance tax.",
             "New York City has raised a taxes:
             an income tax and a sales tax.")

mydict <- c("tax", "income", "capital", "gains", "inheritance")

print(dfm(mytexts, select = mydict))
view(dfm(mytexts, select = mydict))
dfm_a <- dfm(mytexts)
df_a <- convert(dfm_a, to = "data.frame")
dfm_b <- dfm(mytexts, select = mydict)
df_b <- convert(dfm_b, to = "data.frame")

# Example: Laver Garry dictionary
# https://rdrr.io/github/kbenoit/quanteda.dictionaries/
# man/data_dictionary_LaverGarry.html
# https://provalisresearch.com/products/content-analysis-software/
# wordstat-dictionary/laver-garry-dictionary-of-policy-position/
# https://github.com/kbenoit/quanteda.dictionaries 
# (other dictionaries such as Hu & Liu sentiment are available!)
lgdict <- dictionary(file = "LaverGarry.cat", format = "wordstat")

#Laver and Garry dictionary was developed to estimates the policy positions
# of political actors in the United Kingdom 
#by comparing their speeches and written documents to key words found
#in the British Conservative and Labour manifestos of 1992. 
#Note: Please remember that this dictionary was customized to reflect
#the policy positions of UK political parties.

# What's in this thing? 
lgdict
# The dictionary contains 415 words and word patterns. 
# The dictionary has two levels of nesting with 7 main policy areas 
# (level 1) divided up into 19 sub-categories (level 2).

# Run the conservative manifestos through this dictionary
manifestos_lg <- dfm(manifestos_df$text, dictionary = lgdict)

# how does this look
as.matrix(manifestos_lg)[1:5, 1:5]
view(manifestos_lg)
featnames(manifestos_lg)

# plot it
plot(manifestos_df$year, 
     manifestos_lg[,"VALUES.CONSERVATIVE"],
     xlab="Year", ylab="Conservative values", type="b", pch=19)

plot(manifestos_df$year, 
     manifestos_lg[,"INSTITUTIONS.CONSERVATIVE"] 
     - manifestos_lg[,"INSTITUTIONS.RADICAL"],
     xlab="Year", ylab="Net Conservative Institutions", type="b", pch=19)

# RID Dictionary--Regressive Imagery Dictionary
# https://www.kovcomp.co.uk/wordstat/RID.html (multiple languages available!)
#The English Regressive Imagery Dictionary (RID) is composed of about
# 3200 words and roots assigned to 29 categories of primary process cognition,
# 7 categories of secondary process cognition, and 7 categories of emotions.
rid_dict <- dictionary(file = "RID.cat", format = "wordstat")
rid_dict

data("data_corpus_sotu")

sotus_texts <- texts(data_corpus_sotu)

# Get the docvars from the corpus object
year <- (data_corpus_sotu$Date)

sotu_rid_dfm <- dfm(data_corpus_sotu, dictionary = rid_dict)
view(sotu_rid_dfm)

# Look at the categories
featnames(sotu_rid_dfm)

# Inspect the results graphically
plot(year, 
     sotu_rid_dfm[,"PRIMARY.REGR_KNOL.NARCISSISM"],
     xlab="Year", ylab="Narcissism", type="b", pch=19)

plot(year, 
     sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.FIRE"] +
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.ASCEND"] +
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.DESCENT"] +
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.DEPTH"] + 
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.HEIGHT"] + 
       sotu_rid_dfm[,"PRIMARY.ICARIAN_IM.WATER"],
     xlab="Year", ylab="Icarian-ness", type="b", pch=19)

