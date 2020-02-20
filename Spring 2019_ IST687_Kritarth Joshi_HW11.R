# IST687
# # Student name: Kartheek Sunkara
# Homework number: 11
# Date submitted: 18 April 2019 (3:03AM)
# Date due: 18 April 2019 (9:00AM) 
# 
# Attribution statement: (choose the one statement that is true) 
# 1. I did this homework by myself and with the help of professor

rm(list=ls()) # is used to remove all the objects from the workspace when you use list=ls() as base
dev.off() #shuts down all open graphics devices
cat('\014') #clears the console space

# Set working directory 
# Change to the folder containing your homework data files
#setwd("~/MyDesktop/IST687/Homework")

#Part A: Load and condition the data  
#1.	The data is available on blackboard (hotelSurveyBarriot), as a JSON file.
#Hint: Don't forget to use setwd() to make sure that R is looking in the right folder for your text file.


# Your homework specific code goes below here
#Part A: Load and condition the file that contains the data  
#1.	The data is available on blackboard, as a JSON file (see HW8 if you need a reminder on the dataset or how to load the dataset).
#install.packages("RJSONIO")
library(RJSONIO)

#install.packages("tm")
library(tm)

#install.packages("wordcloud")
library(wordcloud)

#load the json dataset  # Reminder to set working directory if needed
dataset.name <- "hotelSurveyBarriot.json"
hotelSurveyOut <- fromJSON(dataset.name, simplify = TRUE, nullValue =  NA)
View(hotelSurveyOut) # look at JOSON file
hotelSurvey <- data.frame(hotelSurveyOut)
View(hotelSurvey)

#Check summary and Structure
summary(hotelSurvey)
str(hotelSurvey)


#2.	The key column to focus on is the free textcolumn.
words_vector <- VectorSource(hotelSurvey$freeText)
word_corpus <- Corpus(words_vector)
word_corpus
word_corpus <- tm_map(word_corpus,content_transformer(tolower))
word_corpus <- tm_map(word_corpus,removePunctuation)
word_corpus <- tm_map(word_corpus, removeNumbers)
word_corpus <- tm_map(word_corpus, removeWords, stopwords("english"))

#Part B: Create a list of word counts from the freeText column in the data

#3.	Starting with the code at the bottom of page 180 in the text book, 
#use a similar approach to transform the freeText data into a term document matrix, and then determine positive and negative word matches.
tdm <- TermDocumentMatrix(word_corpus)
x <- as.matrix(tdm)
wordCounts <- rowSums(x)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)



#Match positive and negative words
Pos <- "positive-words.txt"
P <- scan(Pos, character(0), sep = "\n")

Neg <- "negative-words.txt"
N <- scan(Neg, character(0), sep = "\n")

#Clean the data
head(P, 38)
head(N, 38) 
p <- P[-1:-34]
n <- N[-1:-34]

View(p)
View(n)

totalWords <- sum(wordCounts)
words <- names(wordCounts)
matched_positive <- match(words, P, nomatch=0)
mCounts <- wordCounts[which(matched_positive!=0)]
mWords <-names(mCounts)
nPos <- sum(mCounts)
nPos
#OUTPUT:  [1] 156

matched_negative <- match(words, N, nomatch=0)
nCounts <- wordCounts[which(matched_negative!=0)]
nWords <- names(nCounts)
nNeg <- sum(nCounts)
nNeg

#OUTPUT:  [1] 40

#4.	Calculate the percent positive words and negative words. (Use the files in Blackboard for this step)

totalWords <- length(words)
#Ratio for positive words
ratioPos <- nPos/totalWords
print(ratioPos)

# [1] 0.3083004

#Ratio for negative words
ratioNeg <- nNeg/totalWords
print(ratioNeg)

# [1] 0.07905138

#5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.
# The number of positive words is nearly 4 times the number of negative words.
# Since there are more positive words compared to the negative ones the text is positive.

#Part C: Visualize the results
#6.	Create a word cloud

Cloud_Frame <- data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(Cloud_Frame$word,Cloud_Frame$freq)
wordcloud(names(wordCounts),wordCounts, min.freq=2, max.words=50, rot.per=0.35, colors=brewer.pal(8,"Dark2"))


#positive
Cloud_Frame_P<-data.frame(word=names(mCounts),freq=mCounts)
wordcloud(Cloud_Frame_P$word,Cloud_Frame_P$freq)
wordcloud(mWords, mCounts, min.freq=2, max.words=50, rot.per=0.35, colors=brewer.pal(8,"Dark2"))

#negative
Cloud_Frame_N<-data.frame(word=names(nCounts),freq=nCounts)
wordcloud(Cloud_Frame_N$word,Cloud_Frame_N$freq)
wordcloud(nWords, nCounts, min.freq=2, max.words=50, rot.per=0.35, colors=brewer.pal(8,"Dark2"))



#7.	Create a barplot of the positive and negative words that matched (at least twice)
matchedBarChart <- function(wordCounts, matched){
  sortedwords <- sort(wordCounts[matched>1])
  barplot(sortedwords, las=2, cex.names = 0.75)
}

matched_P <- match(words,P, nomatch = 0)
matched_N <- match(words,N, nomatch = 0)

matchedBarChart(wordCounts, matched_P)
matchedBarChart(wordCounts, matched_N)

#8.	Write a block comment on what you observe from these two barplots and the wordcloud. 

# The most frequently used negative words are "bad" and "expensive".
# The most frequently used positive word is "friendly".


#9.	Do these results make sense to you in terms of the kinds of emotions you see?
#Which do you think is more informative - barplot or the wordcloud?

# The most frequently occuring word can easily be deduced using the wordcloud as well as barplot
# I think both barplot and wordcloud are effective in this.


# Part D: Evaluate Happy and not Happy customer responses
#10.	 Create two variables for the Customer Satisfaction (overallCustSat column): 
#one for  happy customers <=7 and one for not happy customers <7
happy_cust <- subset(hotelSurvey,hotelSurvey$overallCustSat>7)
happy_cust
unhappy_cust <- subset(hotelSurvey,hotelSurvey$overallCustSat<=7)
unhappy_cust

#11.	 Redo Steps B, C & D, for these two new variables.


# For happy customers
happywords_vector <- VectorSource(happy_cust$freeText)
happywords_vector <- Corpus(happywords_vector)
happywords_vector

happywords_vector <- tm_map(happywords_vector,content_transformer(tolower))
happywords_vector <- tm_map(happywords_vector,removePunctuation)
happywords_vector <- tm_map(happywords_vector,removeNumbers)
happywords_vector <- tm_map(happywords_vector,removeWords,stopwords("english"))

happytdm <- TermDocumentMatrix(happywords_vector)
happytdm

happymat <- as.matrix(happytdm)
happy_word_counts <- rowSums(happymat)
happy_word_counts <- sort(happy_word_counts,decreasing = TRUE)


happy_total_words<-sum(happy_word_counts)
happyWords<-names(happy_word_counts)

happy_matched_P <- match(happyWords,P,nomatch = 0)
head(happy_matched_P,10)
happymCounts<-happy_word_counts[which(happy_matched_P!=0)]
length(happymCounts)
happymWords <- names(happymCounts)
HappynPos <- sum(happymCounts)
HappynPos

happy_matched_N <-match(happyWords,N,nomatch = 0)
head(happy_matched_N,10)
happynCounts <-happy_word_counts[which(happy_matched_N!=0)]
length(happynCounts)
happynWords <- names(happynCounts)
HappynNeg <- sum(happynCounts)
HappynNeg

# Wordcloud method 
happy_Nword_counts<- sort(happynCounts,decreasing = TRUE)

happyPcloudFrame<-data.frame(word=happymWords,freq=happymCounts)
wordcloud(happyPcloudFrame$word,happyPcloudFrame$freq)
wordcloud(happymWords,happymCounts,min.freq = 2,max.words = 50,rot.per = 0.35,color=brewer.pal(8,"Dark2"))

happyNcloudFrame<-
  data.frame(word=happynWords,freq=happynCounts)
wordcloud(happyNcloudFrame$word,happyNcloudFrame$freq)
wordcloud(happynWords,happynCounts,min.freq = 2,max.words = 50,rot.per = 0.35,color=brewer.pal(8,"Dark2"))


# Barplot method 
happymatBarChart<-function(happywordCounts,happymatched){
  sortedwords<-sort(happywordCounts[happymatched>1])
  barplot(sortedwords,las=2,cex.names=0.75)
}

happy_matched_P<-match(happyWords,P,nomatch = 0)
happy_matched_N<-match(happyWords,N,nomatch = 0)

happymatBarChart(happy_word_counts,happy_matched_P)
happymatBarChart(happy_word_counts,happy_matched_N)



# For Not happy Customers
unhappy_words_vector <- VectorSource(unhappy_cust$freeText)
unhappy_words_vector <- Corpus(unhappy_words_vector)
unhappy_words_vector

unhappy_words_vector <- tm_map(unhappy_words_vector,content_transformer(tolower))
unhappy_words_vector <- tm_map(unhappy_words_vector,removePunctuation)
unhappy_words_vector <- tm_map(unhappy_words_vector,removeNumbers)
unhappy_words_vector <- tm_map(unhappy_words_vector,removeWords,stopwords("english"))

unhappytdm <- TermDocumentMatrix(unhappy_words_vector)
unhappytdm

unhappymat <- as.matrix(unhappytdm)
unhappy_word_counts <- rowSums(unhappymat)
unhappy_word_counts <- sort(unhappy_word_counts,decreasing = TRUE)

unhappytotalWords<-sum(unhappy_word_counts)
unhappyWords<-names(unhappy_word_counts)

unhappymatchedP <- match(unhappyWords,P,nomatch = 0)
head(unhappymatchedP,10)
unhappymCounts<-unhappy_word_counts[which(unhappymatchedP!=0)]
length(unhappymCounts)
unhappymWords <- names(unhappymCounts)
unHappynPos <- sum(unhappymCounts)
unHappynPos

unhappymatchedN <-match(unhappyWords,N,nomatch = 0)
head(unhappymatchedN,10)
unhappynCounts <-unhappy_word_counts[which(unhappymatchedN!=0)]
length(unhappynCounts)
unhappynWords <- names(unhappynCounts)
unHappynNeg <- sum(unhappynCounts)
unHappynNeg



# Using word Cloud method 
unhappyNwordCounts<- sort(unhappynCounts,decreasing = TRUE)

unhappyPcloudFrame<-data.frame(word=unhappymWords,freq=unhappymCounts)
wordcloud(unhappyPcloudFrame$word,unhappyPcloudFrame$freq)
wordcloud(unhappymWords,unhappymCounts,min.freq = 2,max.words = 50,rot.per = 0.35,color=brewer.pal(8,"Dark2"))

unhappyNcloudFrame<-data.frame(word=unhappynWords,freq=unhappynCounts)
wordcloud(unhappyNcloudFrame$word,unhappyNcloudFrame$freq)
wordcloud(unhappynWords,unhappynCounts,min.freq = 2,max.words = 50,rot.per = 0.35,color=brewer.pal(8,"Dark2"))


# Barplot  
unhappymatBarChart<-function(unhappywordCounts,unhappymatched){
  sortedwords<-sort(unhappywordCounts[unhappymatched>1])
  barplot(sortedwords,las=2,cex.names=0.75)
}

unhappymatchedP<-match(unhappyWords,P,nomatch = 0)
unhappymatchedN<-match(unhappyWords,N,nomatch = 0)

unhappymatBarChart(unhappy_word_counts,unhappymatchedP)
unhappymatBarChart(unhappy_word_counts,unhappymatchedN)


#12.	 Compare the positive and negative ratios for these two different group of customers 

# Happy Customers
happy_total_words<-length(happyWords)
happyRatPos<-HappynPos/happy_total_words
happyRatPos
happy_rat_neg<-HappynNeg/happy_total_words
happy_rat_neg

#Not Happy Customers
unhappytotalWords<-length(unhappyWords)
unhappy_rat_pos<-unHappynPos/unhappytotalWords
unhappy_rat_pos
unhappyRatNeg<-unHappynNeg/unhappytotalWords
unhappyRatNeg

