# Week 4_updated_code: Updated week 4 code for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 2-21-2022
# Last Updated: 2-12-2022
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia.

library(tidyverse)
library(stylo)
library(gutenbergr)
gutenberg_works()
library(quanteda)
library(quanteda.corpora)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)


#------------------------------
# LEXICAL DIVERSITY MEASURES
#------------------------------

#  Load in data: Irish budget proposals from 2008-2012 ----
# "speeches and document-level variables from the 
# debate over the Irish budget".

data("data_corpus_irishbudgets")
irish_budget_texts <- as.character(data_corpus_irishbudgets)
View(irish_budget_texts)


# Type Token Ratio 
budget_tokens <- tokens(irish_budget_texts, remove_punct = TRUE) 

# Num tokens per document
num_tokens <- lengths(budget_tokens)

num_types <- ntype(budget_tokens)

view(summary(data_corpus_irishbudgets))
irish_budget_df <- data.frame("num_tokens" = num_tokens, 
                              "num_types" = num_types,
                              "year" = data_corpus_irishbudgets$year,
                              "party" = data_corpus_irishbudgets$party)

irish_budget_df <- irish_budget_df %>% mutate(TTR = num_types / num_tokens)
view(irish_budget_df)

# Mean per-document TTR scores by year, party
TTR_by_year_party <- irish_budget_df %>% group_by(year, party) %>%
  summarise(mean_ttr = mean(TTR, na.rm = TRUE)) %>% ungroup()
View(TTR_by_year_party)

#------------------------------
# COMPLEXITY (READABILITY) MEASURES
#------------------------------

# FRE (https://en.wikipedia.org/wiki/Flesch–Kincaid_readability_tests)
textstat_readability(data_corpus_irishbudgets, "Flesch") %>% head()

textstat_readability(corpus_group(data_corpus_irishbudgets, groups = year),
                     "Flesch") 

textstat_readability(corpus_group(data_corpus_irishbudgets, groups = party),
                     "Flesch")

# Dale-Chall measure
# (https://en.wikipedia.org/wiki/Dale–Chall_readability_formula)

textstat_readability(data_corpus_irishbudgets, "Dale.Chall.old") %>% head()

textstat_readability(corpus_group(data_corpus_irishbudgets, groups = year),
                     "Dale.Chall.old")

textstat_readability(corpus_group(data_corpus_irishbudgets, groups = party),
                     measure = "Dale.Chall.old")

# let's compare each measure
measure_names <- c("Flesch", "Dale.Chall", "SMOG", "Coleman.Liau", "Fucks")

all_readability_measures <- textstat_readability(data_corpus_irishbudgets,
                                                 measure_names)

readability_matrix <- cbind(all_readability_measures$Flesch, 
                            all_readability_measures$Dale.Chall, 
                            all_readability_measures$SMOG, 
                            all_readability_measures$Coleman.Liau, 
                            all_readability_measures$Fucks
)

readability_cor <- cor(readability_matrix, use = "complete.obs")
rownames(readability_cor) <- measure_names
colnames(readability_cor) <- measure_names
readability_cor

##################################
## stylometric analysis with stylo
##################################

# 1.2 Project Gutenberg: http://www.gutenberg.org/wiki/Main_Page
# collection of (machine readable) novels and other texts + they 
# have an R package!
#install.packages("gutenbergr")
# for more info refer to:
# https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html

gutenberg<-gutenberg_works()

# what do they have by Jane Austen?
austen <- gutenberg_works() %>% filter(author == "Austen, Jane")
austen

# download "Emma"
emma <- gutenberg_download(gutenberg_id = 158, 
                           mirror="http://mirrors.xmission.com/gutenberg/")
# add more meta field information
emma <- gutenberg_download(gutenberg_id = 158, 
                           mirror="http://mirrors.xmission.com/gutenberg/",
                           meta_fields = "title")

# emma <- sample_n(emma, 500, replace=TRUE) 
# emma <- paste(unlist(emma$text), sep=" ")
# emma
# emma <- aggregate(emma$text,list(emma$gutenberg_id), paste, collapse=" ")
# table<-tibble(author = "ja", text = unlist(emma, recursive = FALSE))

# some preloaded data
data(novels)
data(galbraith)
data(lee)

# preprocessing
summary(novels)

tokenized.corpus <- txt.to.words.ext(novels, corpus.lang = "English.all",
                                     preserve.case = FALSE)

# the first line of Pride and Prejudice after tokenization...
tokenized.corpus$Austen_Pride[8:30]

# Personal pronouns are often removed in stylometric studies because they
# tend to be too strongly correlated with the specific topic or genre of a text

corpus.no.pronouns <- 
  delete.stop.words(tokenized.corpus,
                    stop.words = 
                      stylo.pronouns(corpus.lang = "English"))

corpus.no.pronouns$Austen_Pride[8:30]

# sampling
# To split the current corpus into non-overlapping samples of
# 20,000 words each...
# sliced.corpus <- make.samples(tokenized.corpus, sampling = "normal.sampling",
#                              sample.size = 20000)
# summary(sliced.corpus)
# Counting frequent features
# extract the 3,000 most frequent features from the corpus...

frequent.features <- make.frequency.list(tokenized.corpus, head = 3000)

# extract a vector for each text or sample, containing the relative
# frequencies of these features, and combine them into a frequency 
# table for the corpus...
freqs <- make.table.of.frequencies(tokenized.corpus,
                                   features = frequent.features,
                                   relative = TRUE)

# culling
# specify the percentage of samples in which a feature should be present in
# the corpus in order to be included in the analysis

culled.freqs <- perform.culling(freqs, culling.level = 80)
 view(culled.freqs)

# analysis

# can do a principal components analysis (1st two PC's). We'll learn more about
# this when we talk about unsuperivised learning later in semester
stylo(frequencies = culled.freqs, analysis.type = "PCR",
      custom.graph.title = "Austen vs. the Bronte sisters",
      write.png.file = FALSE, gui = FALSE)

# classification
setwd("/Users/noeljohnson_laptop/Dropbox/Mac/Desktop/")

# try and determine "true" authorship of Galbraith’s The Cuckoo’s Calling 
# by comparing with with 25 other fantasy novels and thrillers by 4 famous 
# novelists: H. Coben (e.g. Tell No One), C. S. Lewis (e.g. The Chronicles 
# of Narnia), J. R. R. Tolkien (e.g. the Lord of the Rings trilogy) and 
# J. K. Rowling (e.g. the Harry Potter series)

# specify a table with frequencies:
data(galbraith)
freqs <- galbraith

# specify class labels:
training.texts <- c("coben_breaker", "coben_dropshot", "coben_fadeaway",
                    "lewis_battle", "coben_falsemove", "coben_goneforgood",
                    "lewis_caspian", "rowling_casual", "rowling_chamber",
                    "tolkien_lord1", "tolkien_lord2", "coben_nosecondchance",
                    "coben_tellnoone", "lewis_chair", "lewis_horse",
                    "lewis_lion", "lewis_nephew",
                    "rowling_goblet", "rowling_hallows", "rowling_order",
                    "rowling_prince", "rowling_prisoner", "tolkien_lord3")
test.texts <- c("galbraith_cuckoos", "rowling_stone", "lewis_voyage")


# select the training samples:
training.set <- freqs[(rownames(freqs) %in% training.texts),]
# select remaining rows as test samples:
test.set <- freqs[(rownames(freqs) %in% test.texts),]

# results1 <- classify(training.frequencies = training.set, 
#                      test.frequencies = test.set,
#          mfw.min = 50, mfw.max = 50,
#          classification.method = "knn",
#          n = 3, gui = FALSE)
# summary(results1)
# results1$predicted

# perform knn on the Rowling corpus (no sampling)...
results2 <- classify(training.frequencies = training.set,
                     test.frequencies = test.set, mfw.min = 50, mfw.max = 500,
                     mfw.incr = 50, classification.method = "knn", n=3,
                     cv.folds = 10, gui = FALSE)
# summary(results2$cross.validation.summary)
colMeans(results2$cross.validation.summary)

# results are automatically outputted to a log file "final_results.txt".

# rolling classification

# change working directory
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/code/week_4_code/RdlR_for_rolling_classify-master")

rolling.classify(write.png.file = TRUE, classification.method = "svm", mfw = 100,
                 training.set.sampling = "normal.sampling", slice.size = 5000,
                 slice.overlap = 4500)

#------------------------------
# BOOTSTRAPPING
#------------------------------
# there are packages in R that help with bootstrapping: e.g.
# https://cran.r-project.org/web/packages/boot/boot.pdf

# data prep: remove smaller parties (parties with only 1 document)
large_parties <- c("FF","FG","Green","SF")

irbudgetsCorpSub <- corpus_subset(data_corpus_irishbudgets,
                                  (party %in% large_parties))
# head(docvars(irbudgetsCorpSub))

# convert corpus to df 
irbudgets_df <- data.frame(texts = as.character(irbudgetsCorpSub), 
                           party = docvars(irbudgetsCorpSub, "party"),
                           year = as.integer(docvars(irbudgetsCorpSub,
                                                     "year")),
                           stringsAsFactors = FALSE)

# Let's filter out any NAs
irbudgets_df <- na.omit(irbudgets_df)

# mean Flesch statistic per party
flesch_point <- irbudgets_df$texts %>%
  textstat_readability(measure = "Flesch") %>% 
  group_by(irbudgets_df$party) %>% 
  summarise(mean_flesch = mean(Flesch)) %>% 
  setNames(c("party", "mean")) %>% arrange(party)
flesch_point

# ggplot point estimate
ggplot(flesch_point, aes(x = party, y = mean, colour = party)) +
  geom_point() +
  coord_flip() + theme_bw() + 
  scale_y_continuous(breaks=seq(floor(min(flesch_point$mean)), 
                                ceiling(max(flesch_point$mean)), by = 2)
  ) +
  xlab("") + ylab("Mean Flesch Score by Party") + theme(legend.position="none")

# We will use a loop to bootstrap a sample of texts and subsequently
# calculate standard errors

# load version of apply that adds a progress bar
library(pbapply)

# build function to be used in bootstrapping
boot_flesch <- function(party_data){
  N <- nrow(party_data)
  bootstrap_sample <- corpus_sample(corpus(c(party_data$text)),
                                    size = N, replace = TRUE)
  bootstrap_sample<- as.data.frame(as.matrix(bootstrap_sample))
  readability_results <- textstat_readability(bootstrap_sample$V1,
                                              measure = "Flesch")
  return(mean(readability_results$Flesch))
}

# Breaking it down...
 # N <- nrow(irbudgets_df)
 # bootstrap_sample <- corpus_sample(corpus(c(irbudgets_df$text)),
 #                                   size = N, replace = TRUE)
 # bootstrap_sample
 # readability_results <- textstat_readability(bootstrap_sample,
 #                                             measure = "Flesch")
 # mean(readability_results$Flesch)

# set the number of iterations
iters <- 10

# apply function to each party
boot_flesch_by_party <- pblapply(large_parties, function(x){
  sub_data <- irbudgets_df %>% filter(party == x)
  output_flesch <- lapply(1:iters, function(i) boot_flesch(sub_data))
  return(unlist(output_flesch))
})

names(boot_flesch_by_party) <- large_parties

# compute mean and std.errors
party_means <- lapply(boot_flesch_by_party, mean) %>% unname() %>%
  unlist()
# bootstrap standard error = sample standard deviation bootstrap distribution
party_ses <- lapply(boot_flesch_by_party, sd) %>% unname() %>%
  unlist()

# Plot results--party
plot_dt <- tibble(party = large_parties, mean = party_means, ses = party_ses)
# confidence intervals
interval1 <- -qnorm((1-0.9)/2)   # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
# ggplot point estimate + variance
ggplot(plot_dt, aes(colour = party)) +
  geom_linerange(aes(x = party, ymin = mean - ses*interval1,
                     ymax = mean + ses*interval1), 
                 lwd = 1, position = position_dodge(width = 1/2)
  ) +
  geom_pointrange(aes(x = party, y = mean, ymin = mean - ses*interval2,
                      ymax = mean + ses*interval2), 
                  lwd = 1/2, position = position_dodge(width = 1/2), 
                  shape = 21, fill = "WHITE"
  ) +
  coord_flip() + theme_bw() + 
  xlab("") + ylab("Mean Fleisch Score by Party") + 
  ggtitle("Bootstrapped Irish Budget Fleisch Scores by Party") +
  theme(legend.position = "none")



#














# End Code