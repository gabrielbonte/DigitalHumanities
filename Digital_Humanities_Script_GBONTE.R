library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

text <- readLines(file.choose('atws.txt'))

TextDoc <- Corpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

TextDoc <- tm_map(TextDoc, content_transformer(tolower))

TextDoc <- tm_map(TextDoc, removeNumbers)

TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

TextDoc <- tm_map(TextDoc, removeWords, c("'","'d")) 

TextDoc <- tm_map(TextDoc, removePunctuation)

TextDoc <- tm_map(TextDoc, stripWhitespace)


TextDoc <- tm_map(TextDoc, stemDocument)

TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

head(dtm_d, 10)

barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col =brewer.pal(n = 10, name = 'Spectral'), main ="Top 10 most frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=90, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

text <- readLines(file.choose('atws_fifty.txt'))

TextDoc <- Corpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

TextDoc <- tm_map(TextDoc, content_transformer(tolower))

TextDoc <- tm_map(TextDoc, removeNumbers)

TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

TextDoc <- tm_map(TextDoc, removeWords, c("'","'d")) 

TextDoc <- tm_map(TextDoc, removePunctuation)

TextDoc <- tm_map(TextDoc, stripWhitespace)

TextDoc <- tm_map(TextDoc, stemDocument)

TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)

dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

head(dtm_d, 15)

barplot(dtm_d[1:15,]$freq, las = 2, names.arg = dtm_d[1:15,]$word,
        col =brewer.pal(n = 5, name = 'Spectral'), main ="Top 15 most frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.3)

syuzhet_vector <- get_sentiment(text, method="syuzhet")

head(syuzhet_vector)

summary(syuzhet_vector)

bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)

afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

d <- get_nrc_sentiment(text)

head (d,10)

td<-data.frame(t(d))

td_new <- data.frame(rowSums(td[2:253]))

names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

barplot( col =brewer.pal(n = 8, name = 'Purples') ,
         sort(colSums(prop.table(d[, 1:8]))), 
         horiz = TRUE, 
         cex.names = 0.7,
         las = 1, 
         main = "Emotions in Text", xlab="Percentage"
)
