# Web Crawling/Scraping Tutorial
# Author: Alex Taylor
# Credit to Alexander Cardazzi of ODU for the Knicks Roster Example (and for teaching me this in the first place)
# Date Created: 4/19/22
# Date Last Edited: 4/19/22
##########################################

# Clear Workspace
rm(list = ls())

# Load Packages
library(pacman)
p_load(magrittr, rvest, xml2)

# Set Working Directory
wd <- "/Users/noeljohnson_laptop/Dropbox/Mac/Desktop/scraping/html_files"
setwd(wd)

##########################################

# Web Crawling (i.e. Saving HTML to your Computer)

##########################################

# Crawl a Single Page

link <- "https://www.basketball-reference.com/teams/NYK/2019.html"
link %>%
  read_html() -> myHTML

myHTML %>%
  write_html("knicks2019.html")

# Crawl Multiple Pages

for (year in 2010:2019) {
  paste0("https://www.basketball-reference.com/teams/NYK/",year,".html") %>%
    read_html() %>%
    write_html(paste0("knicks",year,".html"))
}

##########################################

# Web Scraping (i.e. Extracting Information from the Saved HTML) Tables

##########################################

# Extract a Single Table

read_html("knicks2019.html") -> myHTML

myHTML %>%
  html_elements("#roster") %>%
  html_table() -> roster
roster %>%
  as.data.frame() -> roster

# Extract Multiple Tables

list.files() -> files # This creates a list of the crawled html files from our working directory
roster <- list() # Creates a list for storing our tables, making combining them later easier

for (file in files) {
  file %>%
    read_html() %>%
    html_elements("#roster") %>%
    html_table() %>%
    as.data.frame() -> temp 
  
  
  temp$file <- file # Adds a file column so we know where each entry came from
  
  roster[[length(roster) + 1]] <- temp # Adds the current table to the end of the roster list
}

rosterFilled <- do.call(rbind, roster)

##########################################

# Web Scraping Non-Tables

##########################################

# Take a look at the 2019 roster again

read_html("knicks2019.html") -> myHTML

myHTML %>%
  html_elements("#roster") %>%
  html_table() %>%
  as.data.frame() -> knicks

# Say we want to add a variable containing the link to each player's individual page (not included in the table)
# Go back to the html and look for "href" items, which hold hyperlinks

#roster > tbody > tr:nth-child(1) > td:nth-child(2) > a

href="/players/a/allenka01.html"

myHTML %>%
  html_elements("#roster") %>%
  html_elements("a") %>%
  html_attr("href") -> links # Here we use html_attr instead of html_table
head(links)

# We accidentally also collected links for the colleges players attended
# Thankfully, all player links have the word "players" in them, and we can remove all other links from the vector

links <- links[grepl("players",links)]
head(links)

# We almost have what we want, but we need to add the rest of the hyperlink first
links <- paste0("https://www.basketball-reference.com", links)

# Now we have our player links and can add them to the data frame!
knicks$playerLinks <- links

# This isn't only limited to hyperlinks. You can use html_attr to access data from any specific html node

##########################################

# Error Handling

##########################################

# Sometimes code breaks, ending our program. If we want the program to skip these cases, we can use tryCatch()

# Let's try to read the html of a page that doesn't exist

"https://www.basketball-reference.com/teams/NYK/3019.html" %>%
  read_html()

# With the tryCatch() command, we put our code in the top half and an error response in the second half

tryCatch({
  "https://www.basketball-reference.com/teams/NYK/3019.html" %>%
    read_html()
}, error = function(e){
  print("404 error ... try again")
})

# If there is no error, the error response is not triggered

tryCatch({
  "https://www.basketball-reference.com/teams/NYK/2019.html" %>%
    read_html()
}, error = function(e){
  print("404 error ... try again")
})

# You can also have the error tell you the problematic page if you're crawling multiple pages

# Sometimes errors get thrown for connectivity issues or problems unrelated to our code. If you're crawling a large number
# of pages, I highly recommend setting up tryCatch()

##########################################

# Final Thoughts

##########################################

# This only scratches the surface of web scraping. Different websites have different structures and
# different ways of organizing their data. Some other things you may run into: JSON, GraphQL, and APIs. There are
# ample resources online for all of these. Hopefully this tutorial gives you the foundational knowledge to know
# what to look for 

















































