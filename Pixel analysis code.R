#Changing the default location of the working directory
setwd("C:\\Users\\skallam\\Documents\\Social Media")

#Loading the tokens for twitter developer account
source("tokens.R")

# Creating a vector with all the package names we need
x <- c('rtweet', 'SnowballC','topicmodels', 'slam','stringr', 'tm','textdata','rmarkdown', 'forcats', 'textstem', 'Matrix','tidytext','dplyr','tidyr', 'wordcloud','ggplot2')
# Checking if the package is already installed
# If it's not, we install it, otherwise we simply load it
lapply(x, function(x) {if (!require(x, character.only=T)) {install.packages(x);require(x)}})

#Creating the tokens 
twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv=FALSE)

# Using the rtweet package, we extract data from Oct 1 to Oct 29 , 2020.
before_release <- search_fullarchive(q="(#pixel5 OR pixel5 OR googlepixel5) lang:en",
                                     n=500,
                                     env_name="SearchFull",
                                     fromDate="202010010000", # Specifying that we want data starting from Oct 1st 2020
                                     toDate = "202010290000", # Specifying that we want data until Oct 29th 2020
                                     token = twitter_token
)

# You can simply use the csv file to load the data instead
# before_release <- read_twitter_csv("before_release.csv")


# Verifying that we have 500 tweets
nrow(before_release)

# Saving this data to a csv file
write_as_csv(before_release, "before_release.csv")

# Checking what fields are present
colnames(before_release)

# There are too many columns, selecting only those that we are interested for our analysis
before_release <- select(before_release, c("status_id", "created_at", "screen_name", "text", "location"))

# Before proceeding to the pre-processing, we are creating some custom stop words specific to our context
# Note : @madebygoogle is the twitter handle for our product
custom_stop_words <- tribble(
  ~word, ~lexicon,
  "#pixel5", "CUSTOM",
  "#pixel", "CUSTOM",
  "@madebygoogle", "CUSTOM",
  "#madebygoogle", "CUSTOM",
  "madebygoogle", "CUSTOM",
  "google", "CUSTOM",
  "pixel", "CUSTOM",
  "pixel5", "CUSTOM",
  "#google","CUSTOM",
  "googlepixel5", "CUSTOM",
  "#googlepixel5", "CUSTOM",
  "#googlepixel", "CUSTOM",
  "googlepixel", "CUSTOM",
  "teampixel", "CUSTOM",
  "#teampixel", "CUSTOM",
  "@google", "CUSTOM",
  "#google", "CUSTOM",
  "t.co", "CUSTOM",
  "pixel5_", "CUSTOM",
  "googleuk", "CUSTOM",
  "5", "CUSTOM"
)

# We are using the snowball lexicon for stopwords and combining it with our custom stop words to create a final list
stop_words_final <- stop_words %>% filter(lexicon=="snowball") %>% 
  bind_rows(custom_stop_words)

# Pre-processing the data
# Tokenization, removing punctuations, converting to lowercase
# Removing URLS, stop words
# Counting the frequency of each word
before_release_clean <- before_release %>% unnest_tokens(output="word",
                                                         input=text,
                                                         token="words",
                                                         drop=FALSE,
                                                         to_lower=TRUE,
                                                         strip_punct = TRUE) %>%
  mutate(word= gsub("http[[:alnum:][:punct:]]*", "", word)) %>% #removing any URLs
  anti_join(stop_words_final) %>%                                            
  count(word, sort=TRUE)

# Creating a word cloud to check what are the words that are most repeated
wordcloud(before_release_clean$word, 
          before_release_clean$n, 
          min.freq=5, # only consider words that have appeared more than 5 times
          max.words=30, # plot a maximum of 30 words
          scale=c(3,1), 
          colors="dark green")

# Now we move on to the actual data after the mobile phone release
# We are extracting 500 tweets each from Nov, Dec and Jan 
# This is just to make sure that we have an equal distribution of tweets
after_release1 <- search_fullarchive(q="(#pixel5 OR pixel5 OR googlepixel5) lang:en",
                                     n=500,
                                     env_name="SearchFull",
                                     fromDate="202010300000", # Specifying that we want data starting from Oct 30 2020
                                     toDate = "202011300000", # Specifying that we want data until Nov 30 2020
                                     token = twitter_token
)

after_release2 <- search_fullarchive(q="(#pixel5 OR pixel5 OR googlepixel5) lang:en",
                                     n=500,
                                     env_name="SearchFull",
                                     fromDate="202012010000", # Specifying that we want data starting from Dec 1 2020
                                     toDate = "202012310000", # Specifying that we want data until Dec 31 2020
                                     token = twitter_token
)

after_release3 <- search_fullarchive(q="(#pixel5 OR pixel5 OR googlepixel5) lang:en",
                                     n=500,
                                     env_name="SearchFull",
                                     fromDate="202101010000", # Specifying that we want data starting from Jan 1 2021
                                     toDate = "202101310000", # Specifying that we want data until Jan 31 2021
                                     token = twitter_token
)

# Combining the data from 3 months
pixel <-  rbind(after_release1, after_release2, after_release3)

# You can simply load the csv file instead
# pixel <- read_twitter_csv("pixel.csv")

# Verifying that we have 1500 tweets
nrow(pixel)

# Loading the data to a csv file for further use
write_as_csv(pixel,"pixel.csv")

# Selecting only the columns we are interested in
pixel <- pixel %>% select("status_id", "created_at", "screen_name", "text", "location")

head(pixel,1)

# Performing some quick analysis of tweets

# Among the 1500 tweets we extracted, we are checking how many of them 
# have location information
length(unique(pixel$location))

# Plotting the top 10 locations from which tweets were posted
pixel %>%
  filter(!is.na(location)) %>%
  filter(location!="<U+0001F1E6><U+0001F1EA><U+0001F1EE><U+0001F1F3>") %>% 
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Top 10 locations from which tweets were posted")

# Plotting number of tweets per week 
ts_plot(pixel, "weeks") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets") +
  theme_minimal()

# Top 10 tweeters
pixel %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10)

# Top 10 hashtags
pixel %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = TRUE) %>%
  filter(str_detect(hashtag, "^#")) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)


# Top 10 mentions
pixel %>% 
  unnest_tokens(mentions, text, "tweets", to_lower = TRUE) %>%
  filter(str_detect(mentions, "^@")) %>%  
  count(mentions, sort = TRUE) %>%
  top_n(10)


# Pre-processing

# words like shouldn't are replaced by should not 
pixel$text <- gsub("n't"," not", pixel$text)
#Removing words starting with @ (mentions)
pixel$text <- gsub("@\\S*", "", pixel$text)
# Removing words starting with # (hashtags)
pixel$text <- gsub("#\\S*", "", pixel$text)
#Removing numbers
pixel$text <- removeNumbers(pixel$text)
# Removing URLs
pixel$text <- gsub("http[[:alnum:][:punct:]]*", "",pixel$text)

#tokenization, removal of punctuations, converting all words to lower case, removal of stop words
# Also removing words that are less than 2 characters in length
pixel_clean <- pixel %>% unnest_tokens(output="word", input=text, token="words", 
                                       drop=FALSE, to_lower=TRUE, strip_punct = TRUE) %>%
  anti_join(stop_words_final) %>%
  filter(nchar(word)>2)

head(pixel_clean,1)

#Lemmatization ; using textstem package
pixel_lemmatized <- pixel_clean %>% mutate(lemma=lemmatize_words(word))

# We can see that many words have been replaced by their root words
pixel_lemmatized %>% select(lemma,word) %>% filter(lemma!=word)

# Calculating word counts
pixel_wordcount <- pixel_lemmatized %>% count(lemma, sort=TRUE)

# Creating a word cloud to check what are the words that are most repeated
wordcloud(pixel_wordcount$lemma, 
          pixel_wordcount$n, 
          min.freq=5, # only consider words that have appeared more than 5 times
          max.words=30, # plot a maximum of 30 words
          scale=c(4,1), 
          colors="steelblue")

# Creating Document term matrix
pixel_dtm <- pixel_lemmatized %>% count(status_id,lemma) %>%
  cast_dtm(document = status_id, term= lemma, value= n , weighting = tm::weightTf)

# Tried and tested with different values, finally selected 0.97
pixel_dtm <- removeSparseTerms(pixel_dtm,0.97)

head(as.matrix(pixel_dtm))

# For LDA to work, we need each tweet to have a loading in atleast one of the terms left. 
# So we are calculating the rowSums for each document(tweet) and removing any tweets with no loading at all
pixel_rowsums<- apply(pixel_dtm,1,sum)

pixel_dtm <- pixel_dtm[pixel_rowsums>0,]

# Allocating 90% of the data as training set and 10% as test set
sample_size <- floor(0.90 * nrow(pixel_dtm))
set.seed(1111)
train_ind <- sample(nrow(pixel_dtm), size = sample_size)
train <- pixel_dtm[train_ind, ]
test <- pixel_dtm[-train_ind, ]

#Testing LDA topic models with 2 to 15 topics
# Looping over with different values of k (number of topics) 
# by training the model on training set and then calculating the perplexity for test set
values=c()
for(i in c(2:15)){
  lda_model <- LDA(train, k = i, method = "Gibbs",
                   control = list(iter = 25, seed = 1111))
  values <- c(values, perplexity(lda_model, newdata = test))
}

# Plotting the perplexity for different k values
plot(c(2:15), values, main="Perplexity for Topics",
     xlab="Number of Topics", ylab="Perplexity")

# Now we have selected k=4, hence we are implementing the LDA to our entire dataset
pixel_lda <- LDA(pixel_dtm, k=4, method = "Gibbs", control=list(seed=1111))

pixel_lda_beta <- tidy(pixel_lda, matrix="beta")
pixel_lda_gamma <- tidy(pixel_lda, matrix="gamma")

# We are checking the top 10 words from each topic based on the beta value
pixel_topics <- pixel_lda_beta %>% group_by(topic) %>% top_n(10, beta) %>% arrange(topic, -beta)

# Visualizing the top 10 terms for each topic
ggplot(pixel_topics, aes(x = term, y = beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  ggtitle("Review Topics")

# Now we will do some sentiment analysis
head(pixel_lemmatized,1)

# the words "not,no, nor, very, too" change the meaning of the words following them hence we keep them
View(slice(stop_words_final, -(165:167),-(173:174)))

#tokenization, removal of punctuations, URLS, converting all words to lower case, removal of stop words
pixel_clean_sentiment <- pixel %>% unnest_tokens(output="word", input=text, token="words", 
                                       drop=FALSE, to_lower=TRUE, strip_punct = TRUE) %>%
  mutate(word= gsub("http[[:alnum:][:punct:]]*", "", word)) %>% #removing any URLs
  anti_join(slice(stop_words_final, -(165:167),-(173:174)))

#Lemmatization ; using textstem package
pixel_lemma_sentiment <- pixel_clean_sentiment %>% mutate(lemma=lemmatize_words(word))

head(pixel_lemma_sentiment,1)
get_sentiments("afinn")

#Calculating the previous word for each word
pixel_lemma_sentiment <- 
pixel_lemma_sentiment %>% select(status_id, lemma) %>% group_by(status_id) %>%
  mutate(prev_word = lag(lemma,1)) %>% ungroup()
  
# Joining with afinn dictionary to get the sentiment for each word
# Using inner join, so it returns only those words that are present in the dictionary
# If the prev_word is negation word, multiply the sentiment value with -1
# If the prev_word is "very" or "too", increase the value of the sentiment
# Calculate the overall sentiment of each tweet
# then calculate the number of tweets for each value of overall sentiment
pixel_afinn <-   
pixel_lemma_sentiment %>% inner_join(get_sentiments("afinn"),by=c("lemma"="word")) %>% 
  mutate(value = ifelse(prev_word %in% c("not", "no", "nor"), value*-1, value)) %>%
  mutate(value = ifelse(prev_word %in% c("too","very"),
                            ifelse(value==0,0, ifelse(value>0, value+1, value-1)),
                                   value)) %>%
  group_by(status_id) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup() %>%
  count(sentiment , sort=TRUE)

pixel_afinn

# Visualizing the above output
ggplot(pixel_afinn, aes(x=sentiment,y=n)) + 
  geom_col(color='black', fill='steelblue') +
  scale_x_continuous(name="Sentiment scale", limits=c(-8,16), breaks = seq(-8,16,2)) +
  labs(y="Number of tweets",
       title="Sentiment Analysis Afinn")

# Creating sentiment groups for positive, negative, neutral and 
# calculating number of tweets in each case
pixel_afinn %>% rename(count=n) %>% mutate(sentiment_group = ifelse(sentiment==0, "neutral", ifelse(sentiment>0, "positive", "negative"))) %>% 
  group_by(sentiment_group) %>%
  summarise(sum(count))

# Now using nrc dictionary
pixel_nrc <-
pixel_lemma_sentiment %>% inner_join(get_sentiments("nrc"),by=c("lemma"="word")) %>% 
  count(sentiment,sort=TRUE) %>%
  mutate(sentiment2 = fct_reorder(sentiment,n))
# Visualizing the output
ggplot(
  pixel_nrc, aes(x = sentiment2, y = n)
) +
  geom_col(color='steelblue', fill='steelblue') +
  coord_flip() +
  ggtitle("Sentiment counts")

# Now using bing, we will see which are the most used words in positive and negative sentiment

pixel_bing <- pixel_lemma_sentiment %>% inner_join(get_sentiments("bing"),by=c("lemma"="word")) %>%
  count(lemma, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(
    word2 = fct_reorder(lemma, n)
  )
# Visualizing the same
ggplot(pixel_bing, aes(x = word2, y = n)) +
  geom_col(show.legend = FALSE, fill="steelblue") +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Sentiment Word Counts",
    x = "Words",
    y = "Count"
  )