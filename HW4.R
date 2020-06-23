library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(textdata)
library(data.table)


# Set the working directory
setwd("/Users/singhh/Downloads/CSCI48900")

######### Analyzing R questions on StackOverflow #########
######### Question 1 #########
questions <-read.csv(file="questions.csv")
questions$Title=as.character(questions$Title)
questions$Body=as.character(questions$Body)
# questions$CreationDate=as.character(questions$CreationDate)


#convert into tidy format data frame
questions=as_tibble(questions)
question_text <- questions %>% dplyr::select(CreationDate, Title, Body)

#need to analyze words...un-nest
question_words = questions %>%
  unnest_tokens(word, Title)

# df %>%
#   mutate(year = format(as.Date(CreationDate, format = "%Y-%m-%d"), "%Y")) %>%
#   group_by(year) %>%
#   mutate(words = strsplit(as.character(Title), " ")) %>%
#   unnest() %>%
#   count(year, words);
# df

# reduce dimension and remove stop words
data(stop_words)

question_words_no_stop <- question_words %>%
  anti_join(stop_words)


# song frequency by artist in dataset
question_words_no_stop <- question_words_no_stop %>% 
  dplyr::count(word,sort=T) %>% 
  print(n = 20)

# sp<-ggplot(question_words_no_stop, aes(x = word, y = question_words_no_stop)) + geom_point()
# sp

sapply(question_words_no_stop, function(x) length(unique(x)))
data.table(question_words_no_stop)




######### Question 2 #########
library(tm)
library(topicmodels)
library(aRxiv)

questions2 <-read.csv(file="questions.csv")
questions2$Title <- as.character(questions2$Title)

# convert the data into tidy format data frame
questions2=as_tibble(questions2) %>% dplyr::select(Title, Body)


# split abstracts into row for each word
question_words2 = questions2 %>%
  unnest_tokens(word, Title) %>%
  anti_join(stop_words)

# count number of words for each abstract
question_counts2 <- question_words2 %>%
  count(Body, word, sort = TRUE) 

# create document term frequency matrix
question_dtm <- question_counts2 %>%
  cast_dtm(Body, word, n)

##### NOTE: The code below should work but it's giving me out of memory problems ######

# row_totals <- apply(question_dtm, 1, sum) 
# dtm.new   <- question_dtm[row_totals> 0, ] 

# run lda topic model
# question_lda <- tidy(LDA(question_dtm, k = 10, control = list(seed = 1)))

# top terms of each topic
# top_terms <- question_lda %>%
#   group_by(topic) %>%
#   top_n(5, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta) %>%
#   print(n=10)


######### Text classification for song lyrics #########
######### Question 3 #########
library(keras)

songs=read.csv("~/Downloads/CSCI48900/217/songdata.csv")
songs$text=as.character(songs$text)
songs$song=as.character(songs$song)
songs$artist=as.character(songs$artist)


# convert the data tidy format data frame
songs=as_tibble(songs)
song_text <- songs %>% dplyr::select(lyrics = text, song, artist)

# analyze
song_words = songs %>%
  unnest_tokens(word, text)

# reduce dimension and remove stop words
data(stop_words)

song_words_no_stop <- song_words %>%
  anti_join(stop_words)


# song frequency by artist in dataset
songs %>% 
  dplyr::count(artist,sort=T) %>% 
  print(n = 20)


# create document term matrix
corpus <- VCorpus(VectorSource(song_text$song))
tdm <- DocumentTermMatrix(corpus, 
                          list(removePunctuation = TRUE, 
                               stopwords = TRUE,
                               removeNumbers = TRUE))
data_t <- as.matrix(tdm)
data <- cbind(data_t,song_text$artist=="Nirvana")

# split into test and train
ind = sample(2,nrow(data),replace = T,prob = c(0.5,0.5))
# training = data[ind==1,1:(ncol(data)-1)]
# test = data[ind==2,1:(ncol(data)-1)]
traintarget = data[ind==1,ncol(data)]
testtarget = data[ind==2,ncol(data)]

# one hot encoding labels 
trainLabels = to_categorical(traintarget)
testLabels = to_categorical(testtarget)
table(testLabels)

# create neural network
model = keras_model_sequential()
model %>% 
  layer_dense(units=8,
              activation='sigmoid',
              input_shape=c(16))  %>%
  layer_dense(units=2,
              activation='sigmoid')

summary(model)




