# Packages
library(tm) 
library(SnowballC) 
library(e1071)
library(caret)
setwd("C:/Users/NADA/Desktop/RScripts/smsspamcollection")
sms_raw <- read.csv("SMSSpamCollection.txt", stringsAsFactors = FALSE,header = FALSE,sep='\t')
str(sms_raw)
dim(sms_raw)
head(sms_raw)
summary(sms_raw)
sms_raw$V1 <- as.factor(sms_raw$V1)


docs <- Corpus(VectorSource(sms_raw$V2))


docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "english")

docs[[1]]$content
# Create dtm
dtm <- DocumentTermMatrix(docs)
inspect(dtm)
# Split into Train and Test sets
ind <- sample(2, nrow(dtm), replace=TRUE, prob=c(0.7, 0.3))
dtmTrain <- dtm[ind==1,]
dtmTest <- dtm[ind==2,]

trainlabel <- sms_raw[ind==1,]$V1
testlabel <- sms_raw[ind==2,]$V1
sms_freq_words <- findFreqTerms(dtmTrain, 5)
str(sms_freq_words)
sms_dtm_freq_train<- dtmTrain[ , sms_freq_words]
sms_dtm_freq_test <- dtmTest[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# MARGIN = 1 is used for rows
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)
str(sms_train)
dim(sms_train)
classifier <- naiveBayes(sms_train, trainlabel)
sms_test_pred <- predict(classifier, sms_test)
table(testlabel,sms_test_pred)
