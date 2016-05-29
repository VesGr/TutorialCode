###### Working with strings
###### Source for this guide: http://blog.rolffredheim.com/2015/03/digital-data-collection-course.html

setwd("C:/Users/grozeve/Documents/_9_Misc/DataScience/My projects")

install.packages("rvest")
install.packages("knitr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("plyr")
install.packages("jsonlite")
install.packages("stringr")
install.packages("RCurl")

### First, let's learn about string manipulation: tolower, toupper, nchar, substring, str_trim

# From upper to lower case
tolower('ROFL')  
toupper('rofl')
states <- rownames(USArrests)
tolower(states[1:4])

# nchar
nchar(states)
states[nchar(states)==5]

# Substr and substring
substr('abcdef', 2, 4)    # Extracts text between the 2nd and 4th elements, inclusive (e.g., bcd)
substring('abcdef', 2, 4) # Does the same
substring('abcdef', 2, 4:5) # Produces two extractions, one ending at 4th element, the other at 5th element

# str_split
library(stringr)
link <- "http://stats.grok.se/json/en/201401/web_scraping"
str_split(link,'/')    # returns a list
unlist(str_split(link,'/'))

# str_trim
annoyingString <- "\n    something HERE  \t\t\t"
str_trim(annoyingString)

# Practice #1: Parsing through a text file - reading certain lines, splitting them into words, finding repeated words
library(RCurl)
#download.file('https://raw.githubusercontent.com/fredheir/WebScraping/gh-pages/Lecture1_2015/text.txt',destfile='tmp.txt',method='curl')
text <- readLines('tmp.txt')
head(text)
length(text)
a <- text[7]
a
asplit <- unlist(str_split(a, " "))
head(asplit)
length(asplit)
acount <- sort(table(asplit)[table(asplit)>1])
nchar(names(acount))

# grep - finds an element in a list and displays its row #
grep("Ohio", states)
grep("y", states)
states[grep("y", states)]
grep("y", states, invert=T)
grep("y", states, ignore.case=T)
grep("y", states, value=T)

stalinwords <- c("stalin","stalingrad","Stalinism","destalinisation")
grep("stalin", stalinwords,value=T)  # it does not matter where the element appears
grep("[Ss]talin",stalinwords,value=T) # accepts lower and upper case for S in 'Stalin'
grep("s*grad",stalinwords,value=T)    # wildcard 
grep('\\<d', stalinwords,value=T)  # searches for a word that starts with 'd'
grep('d\\>', stalinwords,value=T)  # searches for a word that ends in 'd'

# Practice #2: Use Grep to find all the statements including the words: London, conspiracy, amendment
# Each of the statements in our parliamentary debate begin with a paragraph sign(§) - 
# Use grep to select only these lines. How many separate statements are there?
# Use the caret(^ ) character to match the start of a line. How many lines start with the word 'Amendment'?
# Use the dollar($) sign to match the end of a line. How many lines end with a question mark?
grep("London", text)     # Only one occasion, line 32
grep("conspiracy", text, ignore.case=T)        # 2 occasions, lines 41 and 46
length(grep("amendment", text, ignore.case=T)) # 10 occasions
length(grep("§", text))  # 19 statements
length(grep('^Amendment', text))  # 2 lines start with the word Amendment
grep('^Amendment', text, value=T) # Let's check
length(grep('\\?$', text)) # 6 lines end with a question mark
grep('\\?$', text, value=T)[1:2] # Let's check

# regex
regexpr("one", c("a test", "a basic string", "and one that we want", "one two three")) 
# Each element gives the position of the starting character of the match, if it exists, and -1 when no match occurs for that string.

# gsub
author <- "By John Steinbeck"
gsub("By ", "", author)
gsub("John Steinbeck", "Miroslav Penkov", author)

# paste and paste0 - converts R objects into character vectors
paste(1:10)   # does the same as 'as.character'
paste0(1:10, c("st", "nd", "rd", rep("th", 7)))  # produces a character vector of "1st" "2nd" etc
paste(1:10, c("st", "nd", "rd", rep("th", 7)))   # paste adds space between the elements
var <- 124
paste("url", rep(var, 3), sep="_")
paste("url", 1:4, var, sep="_")
var <- c(123, 421)
paste(var, collapse="_")
paste('a', 1:10, sep="")

### Now, let's do some basic scraping

# Practice writing links efficiently
var <- 201401
paste("http://stats.grok.se/json/en/", var, "/web_scraping")  # note the spaces that were added
myurl <- paste("http://stats.grok.se/json/en/", var, "/web_scraping", sep="")  # no spaces!

# Test the link works (opens up in browser)
browseURL(myurl)
myurl

# Fetch the data if using JSON
raw.data <- readLines(myurl, warn="F") 
raw.data
require(jsonlite)
rd <- fromJSON(raw.data)   # Decomposes the data elements in a list
rd.views <- unlist(rd$daily_views)   # Extracts the daily_views as a vector
rd.views
df <- as.data.frame(rd.views)
df

# All of the above together
var <- 201403
myurl <- paste("http://stats.grok.se/json/en/",var,"/web_scraping", sep="")
rd <- fromJSON(readLines(myurl, warn=F))
df <- as.data.frame(unlist(rd$daily_views))

# Automating the above as a function

myfunction <- function(var) {
  myurl <- paste("http://stats.grok.se/json/en/",var,"/web_scraping", sep="")
  rd <- fromJSON(readLines(myurl, warn=F))
  df <- as.data.frame(unlist(rd$daily_views))
}

df <- myfunction(201401)
df

# Plot the daily views by date
require(ggplot2)
require(lubridate)
df$date <-  as.Date(rownames(df))  # Add another column with the dates
colnames(df) <- c("views","date")  # Set up column names
ggplot(df,aes(date, views))+
  geom_line()+
  geom_smooth()+
  theme_bw(base_size=20)

# Using loops to apply the above to multiple pages (by date)

for (month in 1:9){
  print(paste("http://stats.grok.se/json/en/2013",0,month,"/web_scraping",sep=""))
}

for (month in 10:12){
  print(paste("http://stats.grok.se/json/en/2013",month,"/web_scraping",sep=""))
}

# Practice with loops and functions:
# Write a loop that prints every number between 1 and 10
for (i in 1:10){
  print(i)
}

# Write a loop that adds up all the numbers between 1 and 5
sum <- 0
for (item in 1:5){
  sum <- sum + item
  if (item == 5) {
    print(sum)
  }
}

# Write a function that takes an input number and returns this number divided by two
myfunction <- function(var){
  return(var/2)
}
myfunction(10)

# Write a function that takes two variables, and returns the sum of these variables
myfunction <- function(var1, var2){
  return(var1 + var2)
}
myfunction(2, 4)

# Write a function that returns the Fibonacci sequence from 1 to 10
fib <- as.numeric(1)
fib[1] <- 1
fib[2] <- 1
myfunction <- function(len){
  for (item in 3:len) {
    fib[item] <- fib[item - 1] + fib[item - 2]
  }
  print(fib)
}
myfunction(10)


# Practice with basic scraping
# Can you make an application which takes a Wikipedia page (e.g. Web_scraping) and returns a plot for the month 201312
# Can you extend this application to plot data for the entire year 2013 (that is for pages 201301:201312)
# Can you expand this further by going across multiple years (201212:201301)
# Can you write the application so that it takes a custom data range?
# If you have time, keep expanding functionality: multiple pages, multiple languages.














