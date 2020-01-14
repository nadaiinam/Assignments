
install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("SnowballC") #for stemming
install.packages("wordcloud")
install.packages("graph")
install.packages("Rgraphviz")
install.packages("topicmodels")
install.packages("devtools")
install.packages("data.table")
require("devtools")
install_github("sentiment140", "okugami79")
library(wordcloud)
library("twitteR")
library("ROAuth")
library("SnowballC")

library("wordcloud")


consumerKey <- ""
consumerSecret <- ""
accessToken <- ""
accessTokenSecret <- ""
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
tweets <- searchTwitter("#ParadisePapers", n=100, lang=NULL)
class(tweets)
length(tweets)
tweets[[1]]

# tweets.df <- do.call("rbind", lapply(tweets, as.data.frame))
tweets.df <- twListToDF(tweets)
dim(tweets.df)
#write DF
write.csv(tweets.df, file = "tweets-paradisePapers.csv")

#### Text Cleaning
library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))


myStopwords <- c(stopwords('english'),"rt", "via", "amp")
myStopwords
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

myCorpus[[1]]$content
tweets[[1]]
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm
#inspecting frequent words
freq.terms <- findFreqTerms(tdm, lowfreq = 10)
freq.terms
# Computing Term Frequencies
term.freq <- rowSums(as.matrix(tdm))
#term.freq <- subset(term.freq, term.freq >= 100)
df <- data.frame(term = names(term.freq), freq = term.freq)
head(df)

##Wordcloud
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]
word.freq 
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 50,
           colors = brewer.pal(8, "Dark2"),
          scale = c(3,.5), random.order = FALSE,
          title.size = 1.5)

#Associations
# which words are associated with 'pakistan'?
findAssocs(tdm, "pakistan", 0.2)

#Sentiment Analysis
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
sentiments[1,]
#list positive tweets
pos_tweets <- sentiments[sentiments$polarity == "positive",]
head(pos_tweets)


