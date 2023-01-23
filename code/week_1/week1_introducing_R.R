# Week 1 introduction to R code for TaD class 
# Author: Noel Johnson (adapted from many others---see below)
# Created: 1-27-2022
# Last Updated: 1-23-2023
# Lab adapted from: Lucia Motolinia, Kevin Munger, Patrick Chester,
# Leslie Huang, Pedro L. Rodriguez, and Lucia Motolinia.

# Before you start:
# Download the files "national_clinton_trump_6_20_2016.csv" and "cps08.csv" 

# TIPS:
# you should always (always!) annotate your code
# DEBUGGING: rubber duck it 
# (see https://en.wikipedia.org/wiki/Rubber_duck_debugging
# and https://rubberduckdebugging.com)
# Google is your friend. Type your question and add "R" to the end of it.
# knitr is useful for problem sets that require showing your code 
# For bigger projects: use a dependency manager
# (packrat https://rstudio.github.io/packrat/) for projects 

#-----------------------------
# SETTING UP
#-----------------------------

# Clearing environment
rm(list = ls())
# also, some version of cmd+shift+0 (for mac) will restart your RStudio
# session and clear the workspace. I use this A LOT.

# Working directory
# return current working directory
getwd()  
# set working directory
setwd("/Users/noeljohnson_laptop/Dropbox/Teaching/TaD_Sp2023/lectures/week_1_code/week_1")

# Installing and loading some useful packages from CRAN
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("xtable")
# install.packages("devtools")

# Installing packages from GitHub
# devtools::install_github("quanteda/quanteda.corpora")
# update.packages() # update packages
# (careful, some codes may not run -> you can use packrat in that case)

library(tidyverse)

# Loading multiple packages in a go
libraries <- c("haven", "fixest")
lapply(libraries, require, character.only=TRUE)

# Loading data
polling_data  <- read.csv("data/national_clinton_trump_6_20_2016.csv",
                          stringsAsFactors = FALSE)

cps08 <- read.csv("data/cps08.csv")

# writing data
write.csv(cps08, "data/cps08_altered.csv", na=".")
write_dta(cps08, "data/cps08_altered.dta")

#-----------------------------
# WORKING WITH DATA
#-----------------------------

i_df <- iris # built-in data
i_df
class(i_df)
head(i_df)

i_tbl <- as_tibble(iris)
class(i_tbl)

i_tbl # or print

i_tbl <- i_tbl[1:5, ] # Subset the first 5 rows
names(i_tbl)
i_tbl$Sepal.Length # Extract by name

i_tbl[["Sepal.Length"]] # Extract by name

i_tbl["Sepal.Length"] # or i_tbl[1]

i_tbl <- as_tibble(iris)
i_df <- as.data.frame(i_tbl)
class(i_df)

cps08 <- as_tibble(cps08)
glimpse(cps08)
print(cps08, n=2)

cps08_selected <- select(cps08, c(age,bachelor,female))
cps08_filtered <- filter(cps08, age==33)

arrange(cps08, bachelor)

rename(cps08, married=bachelor)
glimpse(cps08)


cps08 <- mutate(cps08, agesq = age^2)
glimpse(cps08)


# Fixest can be used to run regression etc...

# Packages, WD, and Data --------------------------------------------------

# Clear workspace
# rm(list = ls())

# Remove scientific notation
options(scipen = 999)

# load libraries
library(pacman)
p_load(tidyverse, fixest)

# set wd
#wd <- "~/Documents/George Mason/Papers/confed_monuments"
#setwd(wd)

# Data (Just Virginia from my dataset)
load("trnt_mon_va.Rda") # Paste your pathfile here


# Fixest Package Info -----------------------------------------------------

# CRAN Page: https://cran.r-project.org/web/packages/fixest/
# User Manual: https://cran.r-project.org/web/packages/fixest/fixest.pdf

# I've linked multiple vignettes from the package creators throughout, those will be the best source of information and applications beyond the
# very basic examples I've included here


# Primary Function: feols -------------------------------------------------

# Using feols is very similar to other lm commands, but with some features simplified and others added

# Basic regression with only your formula
reg1 <- feols(c_dem ~ post + p_urban + mfgwages + avfarmval_p_acre, data = trnt_mon_va)

# Generate table with etable
etable(reg1)

# Fixed Effects (added after the formula, following | )
reg2 <- feols(c_dem ~ post + p_urban + mfgwages + avfarmval_p_acre | icpsr + year, data = trnt_mon_va) # ICPSR is a county code
# when using fixed effects, S.E.s will be clustered on first listed fixed effect

# Table
etable(reg2) # Fixed effects are neatly placed in a second section labelled Fixed-Effects!

# You can also cluster and weight using feols, but be sure to put ~ before your variable names
reg3 <- feols(c_dem ~ post + p_urban + mfgwages + avfarmval_p_acre | icpsr + year, cluster = ~icpsr, weights = ~totpop, data = trnt_mon_va)

# Table
etable(reg3)

# feols can also do IVs, but I haven't done those myself. You add them similarly to fixed effects. Check out the help file ?feols

# A comprehensive intro vignette: https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html

# Table Customization -----------------------------------------------------

# thankfully it's very straightforward, but the help file is huge since it's so customizable.
# Here are some basics I've used

# Generate tex table (code in console)
etable(reg1, tex = T) # You can copy and paste this code into the tex document for your paper

# Generate tex table (save as separate file for easier updating)
# etable(reg1, tex = T, file = "./Figures/filename_here.tex", replace = T) # You can insert this file into your tex document, making updates automatic
# Always do replace = T unless you want a bunch of tables stacked together in one file!

# You can report only specific variables using keep =
etable(reg1, keep = "post")
etable(reg1, keep = c("post","p_urban"))

# You can change variable names to more descriptive names using dict =, but be sure to update keep = to match your new dictionary!
etable(reg1, dict = c(c_dem = "% Dem",
                      post = "Post"),
       keep = "Post")

# Definitely check out the help file ?etable for a huge list of other options! Might be better to pull up in an actual browser since it's long

# Super helpful vignette: https://cran.r-project.org/web/packages/fixest/vignettes/exporting_tables.html


# Multiple Models with feols ----------------------------------------------

# It's very simple to run multiple models in feols that are included in the same table

# Multiple LHVs: Just include all desired variables in c() on the left-hand side
mult_reg1 <- feols(c(c_dem, c_tov, pb) ~ post + p_urban + mfgwages + avfarmval_p_acre, data = trnt_mon_va)

# Table
etable(mult_reg1)

# Multiple RHVs: More options, like sw(), sw0(), csw(), csw0()

# sw() includes each control in a stepwise manner, but only one in each model. sw0() runs an additional model without the included controls
mult_reg2 <- feols(c_dem ~ post + sw(p_urban, mfgwages, avfarmval_p_acre), data = trnt_mon_va)

# Table
etable(mult_reg2)

#csw() is cumulative stepwise, keeping each control and adding the next one in a stepwise manner. csw0() works the same as in the above case
mult_reg3 <- feols(c_dem ~ post + csw(p_urban, mfgwages, avfarmval_p_acre), data = trnt_mon_va)

# Table
etable(mult_reg3)

# For more info, check out this vignette: https://cran.r-project.org/web/packages/fixest/vignettes/multiple_estimations.html

# All of the above table customizations also apply to these multiple estimation tables. Makes running multiple models compiled into one table
# substantially easier than anything else I've tried. While other packages handle regressions similarly well, the etable() command just clicked with
# me and has made fixest my preferred package for doing analysis that I need to export in a tex table

########


# Basic R graphics
attach(mtcars) # pre-loaded data
plot(wt, mpg, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x) 
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)

# plot it in ggplot
# Basic scatter plot
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

# change points to labels
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_text(label=rownames(mtcars))

# Add the regression line
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point() +
  geom_smooth(method=lm)
# Remove the confidence interval
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
# Loess method
plot <- ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point()+
  geom_smooth()

plot

# take a look at plotly for interactive plots: https://plot.ly/r/

# Exporting graph
# Exporting graph to pdf
pdf(width = 4, height = 3, "plot1.pdf")
plot
dev.off()

#-----------------------------
# LOOP & FUNCTIONS
#-----------------------------

# For Loops

# A loop that identifies and stores variables that contain characters
for(col_name in names(polling_data)){ 
  if(is.character(polling_data[, col_name])) {
    print(col_name)
  }
}

# Apply functions (with regex (regular expressions))
names(polling_data) <- sapply(names(polling_data), function(i) {
  i <- gsub("\\.", "_", i) # Replaces all instances of "." with an "_"
  i <- gsub("__", "_", i) # Replaces all instances of "__" with "_"
} )

# easy to apply base functions
sapply(polling_data[,c("Clinton", "Trump")],
       mean)   
# mean does not work with NAs
sapply(polling_data[,c("Clinton", "Trump", "Undecided")],
       mean) 
# need to specify how to deal with NAs (this is an argument of mean)
sapply(polling_data[,c("Clinton", "Trump", "Undecided")],
       mean, na.rm = TRUE)
# alternative
sapply(polling_data[,c("Clinton", "Trump", "Undecided")],
       function(x) mean(x, na.rm = TRUE)) 

# output of sapply is a vector or a matrix
mean_vars1 <- sapply(polling_data[,c("Clinton", "Trump", "Undecided")],
                     function(x) mean(x, na.rm = TRUE)) 
# output of lapply is a list
mean_vars2 <- lapply(polling_data[,c("Clinton", "Trump", "Undecided")],
                     function(x) mean(x, na.rm = TRUE)) 

# the apply is useful when applying a function to rows OR columns
apply(polling_data[,c("Clinton", "Trump")], 2, mean, na.rm = TRUE) # 2 = columns
apply(polling_data[,c("Clinton", "Trump")], 1, mean, na.rm = TRUE) # 1 = rows

## dplyr version
# polling_data %>%
# summarise(avg.clinton = mean(Clinton), avg.trump = mean(Trump, na.rm = TRUE))
# polling_data %>% rowwise() %>% 
# summarise(avg.row = mean(Clinton, Trump, na.rm = TRUE))
  
## User written functions
# Calculates the cosine similarity between two vectors
# %*% specifies dot product rather than entry by 
# entry multiplication (we could also do: sum(x * y))
calculate_cosine_similarity <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  return(nominator/denominator)
}

set.seed(1984L)  # allows us to replicate result
x  <- rnorm(10) # Creates a vector of random normally distributed numbers
y  <- x*2 + 3

calculate_cosine_similarity(x,y)


#-----------------------------
# FINISHING UP
#-----------------------------

# You can save a workspace after running it -- all objects, functions, 
# etc  (e.g. if you have run something computationally intensive and 
# want to save the object for later use)
# Similar to pickle() in Python

save.image("workspace.RData")

# Pick up where you left off (but note that the workspace 
# does not include packages. You need packrat for that)

rm(list = ls())

load("workspace.RData")

#-----------------------------
# FREE RESOURCES
#-----------------------------

# UCLA
# http://www.ats.ucla.edu/stat/r/

# Rbloggers
# https://www.r-bloggers.com/how-to-learn-r-2/

# Data Camp
# https://www.datacamp.com/

# Swirl
# http://swirlstats.com/

# If you have a question, it's probably been asked before
# on StackOverflow/StackExchange!






# end code


