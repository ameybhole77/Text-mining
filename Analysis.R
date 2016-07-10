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

# Make tokenizer function 
tokenizer = function(x)
  NGramTokenizer(x , Weka_control(min =2 ,max =2))

# Create amzn_p_tdm
amzn_p_tdm = TermDocumentMatrix(amzn_pros_corp , control = list(tokenize = tokenizer))

# Create amzn_p_tdm_m
amzn_p_tdm_m = as.matrix(amzn_p_tdm)

# Create amzn_p_freq
amzn_p_freq = rowSums(amzn_p_tdm_m)

# Plot a wordcloud using amzn_p_freq values
wordcloud(names(amzn_p_freq) , max.words =25 , color = "blue")

# Create amzn_c_tdm
amzn_c_tdm = TermDocumentMatrix(amzn_cons_corp , control = list(tokenize =tokenizer))

# Create amzn_c_tdm_m
amzn_c_tdm_m = as.matrix(amzn_c_tdm)

# Create amzn_c_freq
amzn_c_freq = rowSums(amzn_c_tdm_m)

# Plot a wordcloud of negative Amazon bigrams
wordcloud(names(amzn_c_freq) , max.words = 25 , color = "red")

# Create amzn_c_tdm
amzn_c_tdm = TermDocumentMatrix(amzn_cons_corp , control = list(tokenize = tokenizer))

# Print amzn_c_tdm to the console
amzn_c_tdm

# Create amzn_c_tdm2 by removing sparse terms 
amzn_c_tdm2 = removeSparseTerms(amzn_c_tdm , sparse = 0.993)

# Create hc as a cluster of distance values
hc = hclust(dist(amzn_c_tdm2 ,method = "euclidean") , method ="complete" )

# Produce a plot of hc
plot(hc)

# Create amzn_p_tdm
amzn_p_tdm = TermDocumentMatrix(amzn_pros_corp , control = list(tokenize = tokenizer))

# Create amzn_p_m
amzn_p_m = as.matrix(amzn_p_tdm)

# Create amzn_p_freq
amzn_p_freq = rowSums(amzn_p_m)

# Create term_frequency
term_frequency = sort(amzn_p_freq , decreasing = TRUE)

# Print the 5 most common terms
term_frequency[1:5]

# Find associations with fast paced
findAssocs(amzn_p_tdm , "fast paced" , 0.2)

# Create total_goog
total_goog = rbind(goog_pros , goog_cons)

# Create goog_source
goog_source = DataframeSource(total_goog)

# Create all_goog
all_goog = VCorpus(goog_source)

# Clean all_goog_corp
all_goog_corp = tm_clean(all_goog)

# Create all_tdm
all_tdm = TermDocumentMatrix(all_goog_corp)

# Name the tdm columns
colnames(all_tdm) = c("Google Pros" , "Google Cons")

# Create all_m
all_m = as.matrix(all_tdm)

# Build a comparison cloud
comparison.cloud(all_m , colors = c("#F44336", "#2196f3") , max.words = 100)

# Structure of Amazon pros
str(amzn_pros)

# Structure of Google pros
str(goog_pros)

# Create total_pros
total_pros = rbind(amzn_pros , goog_pros)

# Create pros_source
pros_source = DataframeSource(total_pros)

# Create all_pros
all_pros = VCorpus(pros_source)

# Create all_pros_corp
all_pros_corp = tm_clean(all_pros)

# Create bigram TDM 
all_pros_tdm = TermDocumentMatrix(all_pros_corp , control = list(tokenize = tokenizer))

# Name the columns
colnames(all_pros_tdm) = c("Amazon Pros" , "Google Pros")

# Create all_tdm_pm 
all_tdm_pm = as.matrix(all_pros_tdm)

# Create common_words
common_pros_words <- subset(all_tdm_pm,
                            all_tdm_pm[, 1] > 0 & all_tdm_pm[, 2] > 0) 

# Create difference
difference = abs(common_pros_words[, 1] - common_pros_words[, 2]) 

# Add difference to common_pros_words
common_pros_words= cbind(common_pros_words, difference)

# Order the data frame from most differences to least
common_pros_words  = common_pros_words[order(common_pros_words[, 3],
                                             decreasing = TRUE), ] 
# Create top15_df_pros
top15_df_pros = data.frame (x = common_pros_words[1:15, 1] , 
                            y = common_pros_words[1:15, 2] ,
                            labels = rownames(common_pros_words[1:15 , ]))

# Create the pyramid plot
pyramid.plot(top15_df_pros$x, top15_df_pros$y, labels  = top15_df_pros$labels,
             gap = 12, main = "Words in Common", unit = NULL,
             top.labels = c("Amazon", "Pro Words", "Google"))

# Structure of Amazon cons
str(amzn_cons)

#Structure of Google cons
str(goog_cons)

# Create total_cons
total_cons = rbind(amzn_cons , goog_cons)

# Create cons_source
cons_source = DataframeSource(total_cons)

# Create all_cons
all_cons = VCorpus(cons_source)

# Create all_cons_corp
all_cons_corp = tm_clean(all_cons)

# Create bigram TDM
all_cons_tdm = TermDocumentMatrix(all_cons_corp , control = list(tokenize = tokenizer))

# Name the columns
colnames(all_cons_tdm) = c("Amazon Cons" , "Google Cons")

# Create all_tdm_cm
all_tdm_cm = as.matrix(all_cons_tdm)


# Create common_cons_words
common_cons_words <- subset(all_tdm_cm,
                            all_tdm_cm[, 1] > 0 & all_tdm_cm[, 2] > 0) 

# Create difference
difference = abs(common_cons_words[, 1] - common_cons_words[, 2]) 

# Add difference to common_cons_words
common_cons_words= cbind(common_cons_words, difference)

# Order the data frame from most differences to least
common_cons_words  = common_cons_words[order(common_cons_words[, 3],
                                             decreasing = TRUE), ] 
# Create top15_df_cons
top15_df_cons = data.frame (x = common_cons_words[1:15, 1] , 
                            y = common_cons_words[1:15, 2] ,
                            labels = rownames(common_cons_words[1:15 , ]))

# Create the pyramid plot
pyramid.plot(top15_df_cons$x, top15_df_cons$y, labels  = top15_df_cons$labels,
             gap = 12, main = "Words in Common", unit = NULL,
             top.labels = c("Amazon", "Con Words", "Google"))

# Find Associations
findAssocs(amzn_p_tdm, "fast paced", 0.2)[[1]][1:15]
