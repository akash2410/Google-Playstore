#Setting up the working directory
setwd("C://Users//Akash Tiwari//Desktop//google Playstore")

#importing all those necessary libraries
library(tm)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)

#reading data
reviews <- read.csv("googleplaystore_user_reviews.csv", sep = ',')

#little data manipulaton
reviews <- reviews %>% select(Translated_Review) %>%
  filter(Translated_Review != "nan") %>%
  mutate(doc_id = 1) %>%
  select(doc_id, Translated_Review) %>%
  rename(text = Translated_Review)
reviews %>% head   

#Let's take a look at the structure of data
str(reviews$text)

#converting text from factors to stings
reviews$text <- as.character(reviews$text)

#Calculating Length of each reviews 
reviews$text_length <- nchar(reviews$text)

#Let's plot the text length
ggplot(reviews, aes(text_length))+
  geom_histogram(binwidth = 15)

#Creating corpus
reviews_corpus <- Corpus(VectorSource(reviews$text))
print(reviews_corpus)

inspect(reviews_corpus[1:4])

#Cleaning data for Text mining
clean_corpus <- tm_map(reviews_corpus, tolower)
clean_corpus <- tm_map(clean_corpus, removePunctuation)
clean_corpus <- tm_map(clean_corpus, removeNumbers)
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords("english"))
clean_corpus  <- tm_map(clean_corpus, stripWhitespace)

inspect(clean_corpus[1:4])

#TermDocumentMatrix for corpus
clean_dtm <- DocumentTermMatrix(clean_corpus)

inspect(clean_dtm[1:10, 10:15])

dtm <- as.matrix(clean_dtm)

#Creating a WordCLoud
wordcloud (clean_corpus, scale=c(4,0.5), max.words=70, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

#Let's take a look at words which occurs for more than 1000 times
findFreqTerms(clean_dtm, lowfreq = 100)

#Sentiment Analysis
tokens <- data_frame(text = reviews$text) %>% unnest_tokens(word, text)
tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words
