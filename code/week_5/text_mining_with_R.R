# Code from "Text Mining with R"
# Date: 2-21-23
# Last Edited: 2-21-23
# Author: Noel Johnson

#############
# Chapter 1: The tidy text format
#############

library(tidyverse)
library(tidytext)

# The unnest_tokens function
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

# make a dataframe
text_df <- tibble(line = 1:4, text = text)
# if I didn't specify "line" I would get the variable "line"
# text_df <- tibble(text = text)

text_df

# Notice that this data frame containing text isn’t yet compatible with 
# tidy text analysis, though. We can’t filter out words or count which occur 
# most frequently, since each row is made up of multiple combined words. 
# We need to convert this so that it has one-token-per-document-per-row.

# Within our tidy text framework, we need to both break the text into 
# individual tokens (a process called tokenization) and transform it to a 
# tidy data structure. To do this, we use tidytext’s unnest_tokens() function.

text_df %>%
  unnest_tokens(word, text)

# The two basic arguments to unnest_tokens used here are column names. 
# First we have the output column name that will be created as the text is 
# unnested into it (word, in this case), and then the input column that the 
# text comes from (text, in this case).
# 
# Also notice:
# --Other columns, such as the line number each word came from, are retained.
# --Punctuation has been stripped.
# --By default, unnest_tokens() converts the tokens to lowercase, 
# which makes them easier to compare or combine with other datasets. 
# (Use the to_lower = FALSE argument to turn off this behavior).

# Tidying the works of Jane Austen

library(janeaustenr)

view(austen_books())

# let's add line numbers using mutate and chapter numbers using a regex
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

view(original_books)

# To work with this as a tidy dataset, we need to restructure it in the 
# one-token-per-row format, which as we saw earlier is done with the 
# unnest_tokens() function.

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

# We can remove stop words (kept in the tidytext dataset stop_words) with 
# an anti_join()

# we can list the data sets currently available to us
data()

# we want to use the stop_words data set in the tidytext package
data(stop_words)

# we remove stop words using an anti_join
# anti_join() return all rows from x without a match in y
tidy_books <- tidy_books %>%
  anti_join(stop_words)

# We can also use dplyr’s count() to find the most common words in all the 
# books as a whole.

tidy_books %>%
  count(word, sort = TRUE)

# pipe this directly to the ggplot2 package, for example to create a 
# visualization of the most common words 

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# The gutenbergr package
# provides access to the public domain works from the 
# Project Gutenberg collection
# the function gutenberg_download() is used to download one or more works 
# from Project Gutenberg by ID
library(gutenbergr)

# Word frequencies

# download four works by H.G. Wells
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# tidy the texts
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# what are the most common words?
tidy_hgwells %>%
  count(word, sort = TRUE)

# Now let’s get some well-known works of the Brontë sisters, 
# whose lives overlapped with Jane Austen’s but who wrote in a different style.
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# what are the most common words?
tidy_bronte %>%
  count(word, sort = TRUE)

# let’s calculate the frequency for each word for the works of Jane Austen,
# the Brontë sisters, and H.G. Wells by binding the data frames together. 
# We can use pivot_wider() and pivot_longer() from tidyr to reshape 
# our dataframe 

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Brontë Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")

frequency

# We use str_extract() here because the UTF-8 encoded texts from 
# Project Gutenberg have some examples of words with underscores around them 
# to indicate emphasis (like italics). The tokenizer treated these as words, 
# but we don’t want to count “_any_” separately from “any”, for example.

# let's plot
library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# correlations
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)


#############
# Chapter 2: Sentiment analysis with tidy data
#############

# clear the environment
rm(list = ls())

# Let’s address the topic of opinion mining or sentiment analysis. 

# The sentiments datasets

# there are a variety of methods and dictionaries that exist for evaluating 
# the opinion or emotion in text. The tidytext package provides access to 
# several sentiment lexicons. Three general-purpose lexicons are
# --AFINN from Finn Årup Nielsen,
# --bing from Bing Liu and collaborators, and
# --nrc from Saif Mohammad and Peter Turney.

# NOTE: https://juliasilge.com/blog/sentiment-lexicons/


# All three of these lexicons are based on unigrams, i.e., single words. 
# These lexicons contain many English words and the words are assigned scores 
# for positive/negative sentiment, and also possibly emotions like joy, anger, 
# sadness, and so forth. The nrc lexicon categorizes words in a binary fashion 
# (“yes”/“no”) into categories of positive, negative, anger, anticipation, 
# disgust, fear, joy, sadness, surprise, and trust. The bing lexicon 
# categorizes words in a binary fashion into positive and negative categories. 
# The AFINN lexicon assigns words with a score that runs between -5 and 5, 
# with negative scores indicating negative sentiment and positive scores 
# indicating positive sentiment.

# get_sentiments() allows us to get specific sentiment lexicons from the 
# library textdata
library(tidytext)
library(textdata)

get_sentiments("bing")

library(janeaustenr)
library(tidyverse)
library(stringr)

# we need to take the text of the novels and convert the text to the tidy 
# format using unnest_tokens(). also set up some other columns to keep track 
# of which line and chapter of the book each word comes from; 
# we use group_by and mutate to construct those columns.

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# perform the sentiment analysis. What are the most common positive words 

bing_positive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(bing_positive) %>%
  count(word, sort = TRUE)

library(tidyr)

# Small sections of text may not have enough words in them to get a good 
# estimate of sentiment while really large sections can wash out narrative
# structure. For these books, using 80 lines works well,

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Most common positive and negative words

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

# create a custom stop word dictionary

custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

custom_stop_words


# library(wordcloud)
# 
# tidy_books %>%
#   anti_join(stop_words) %>%
#   count(word) %>%
#   with(wordcloud(word, n, max.words = 100))
# 
# library(reshape2)
# 
# tidy_books %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(word, sentiment, sort = TRUE) %>%
#   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
#   comparison.cloud(colors = c("gray20", "gray80"),
#                    max.words = 100)

# beyond just words

# we may want to tokenize text into sentences, and it makes sense to use a
# new name for the output column in such a case.

p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

p_and_p_sentences$sentence[8]

# The sentence tokenizing does seem to have a bit of trouble with UTF-8 encoded
# text, especially with sections of dialogue; it does much better with 
# punctuation in ASCII.

# Another option in unnest_tokens() is to split into tokens using a regex 
# pattern. We could use this, for example, to split the text of Jane Austen’s
# novels into a data frame by chapter.

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

# First, let’s get the list of negative words from the Bing lexicon. 
# Second, let’s make a data frame of how many words are in each chapter so we 
# can normalize for the length of chapters. Then, let’s find the number of 
# negative words in each chapter and divide by the total words in each chapter.

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% 
  ungroup()

#

























# end code