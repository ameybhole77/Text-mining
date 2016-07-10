# Load packages
library(dplyr)
library(data.table)
library(ggplot2)
library(xlsx)
library(tm)
library(qdap)
library(RWeka)
library(wordcloud)
library(plotrix)

# Load amzn
amzn =  read.xlsx("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/Text Mining/amzn.xlsx" ,sheetName = "amzn" )

# Load goog
goog = read.xlsx("/media/amey/1E02DDE102DDBDC9/Amey/Work/Data A/Assignments/Text Mining/goog.xlsx" , sheetName = "goog" )

# Print the structure of amzn
str(amzn)

# Create amzn_pros
amzn_pros = amzn$pros

# Create amzn_cons
amzn_cons = amzn$cons

# Print the structure of goog
str(goog)

# Create goog_pros
goog_pros = goog$pros

# Create goog_cons
goog_cons = goog$cons

# qdap cleaning function
qdap_clean <- function(x) {
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_number(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  return(x)
}

# tm_clean function
tm_clean <- function(corpus) {
  tm_clean <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords,
                   c(stopwords("en"), "Google", "Amazon", "company"))
  return(corpus)
}

# Alter amzn_pros
amzn_pros = qdap_clean(amzn_pros)

# Alter amzn_cons
amzn_cons = qdap_clean(amzn_cons)

# Create az_p_corp 
az_p_corp = VCorpus(VectorSource(amzn_pros))

# Create az_c_corp
az_c_corp = VCorpus(VectorSource(amzn_cons))

# Create amzn_pros_corp
amzn_pros_corp = tm_clean(az_p_corp)

# Create amzn_cons_corp
amzn_cons_corp = tm_clean(az_c_corp)

# Apply qdap_clean to goog_pros
goog_pros = qdap_clean(goog_pros)

# Apply qdap_clean to goog_cons
goog_cons = qdap_clean(goog_cons)

# Create goog_p_corp
goog_p_corp = VCorpus(VectorSource(goog_pros))

# Create goog_c_corp
goog_c_corp = VCorpus(VectorSource(goog_cons))

# Create goog_pros_corp
goog_pros_corp = tm_clean(goog_p_corp)

# Create goog_cons_corp
goog_cons_corp = tm_clean(goog_c_corp)

