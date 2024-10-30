# Installing packages
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
install.packages("quanteda") # for bigram analysis
install.packages("quanteda.textstats")

# Loading libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")


# Read the text file from local machine , choose file interactively
text <- readLines(file.choose())

# Loading the data as corpus
TextDoc <- Corpus(VectorSource(text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Convert the text to lower case
TextDoc <- tm_map(TextDoc, tolower)
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
TextDoc <- tm_map(TextDoc, removeWords, c("chicken","order","food","sandwich"))

# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by decreasing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

# Display the top 5 most frequent words
head(dtm_d, 30)

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 6,
          max.words = 110, random.order = FALSE, rot.per = 0.40,
          colors = brewer.pal(8, "Dark2"))

# Add title
title(main = "Chick-fil-A Kitchener")

# Word Associations
# Finding associations 
findAssocs(TextDoc_dtm, terms = c("good","service","nuggets", "thru", "overall"), corlimit = 0.20)	
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 100), corlimit = 0.25)

# Sentiment Scores
# Finding Sentiment Scores
syuzhet_vector <- get_sentiment(text, method="syuzhet")
# See summary statistics of the vector
summary(syuzhet_vector)

# Emotion Classification

Emotion_class <- get_nrc_sentiment(text)

head(Emotion_class, 50)

df <- data.frame(t(Emotion_class))
df_1 <- data.frame(rowSums(df))
names(df_1)[1] <- "count"
df_1 <- cbind("sentiment" = rownames(df_1), df_1)
rownames(df_1) <- NULL
df_2 <- df_1[1:10,]
df_2
quickplot(sentiment, data=df_2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Emotion Classification")





