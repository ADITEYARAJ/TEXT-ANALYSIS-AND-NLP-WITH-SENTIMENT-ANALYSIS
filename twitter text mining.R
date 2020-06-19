library("twitteR")
#install.packages("ROAuth")
library("ROAuth")
consumerKey='FXTquJNbgDG2dH81XYVqNZFAb'
consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO'
requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'
accesstoken <- "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh"
accesstokensecret <- "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A"
setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesstokensecret)
tweet <- searchTwitter("$aapl",n=10,lang = "en")
tweet
tweetsdf <- twListToDF(tweet)
write.csv(tweetsdf,file = "tweet.txt",row.names = F)
getwd()
app <- read.csv(file.choose())
View(app)
head(app)
#Trends Locations
trend <- availableTrendLocations()
head(trend)
View(trend)
#getting trend of world
world <- getTrends(1)
View(world)
world
word <- app$text
View(word)
word <- as.character(word)
word.raw <- tokens(word,what="word",remove_number=T,remove_punct = T,remove_symbols = T,split_hyphens=T)
#Lower case the tokens
word.raw <- tokens_tolower(word.raw)
#to remove stopwords
word.raw <- tokens_select(word.raw,stopwords(),selection = "remove")
word.raw <- tokens_wordstem(word.raw,language = "english")
word.raw <- tokens_ngrams(word.raw,n=1:2)
word.raw.dfm <- dfm(word.raw)
word.raw.matrix <- as.matrix(word.raw.dfm)                           
word.raw.matrix1 <- as.data.frame(word.raw.matrix)  
View(word.raw.matrix1)
word.raw.matrix2<- word.raw.matrix1[which(rowSums(word.raw.matrix1) > 0),]
#function for calculating tje tem frequency
term.frequency <- function(row){
  row/sum(row)  
}
#function for calculating inverse document frequency
inverse.doc.freq <- function(col){
  cor.size <- length(col)
  doc.size <- length(which(col>0))
  log10(cor.size/doc.size)
}  
#fun for calculating TF-IDF
tf.idf <- function(tf,idf){
  tf*idf
}
#first normalize all document via tf
word.raw.dfm <- apply(word.raw.matrix2,1,term.frequency)
dim(word.raw.dfm)
#second,calculate IDF
word.raw.idf <- apply(word.raw.matrix2, 2,inverse.doc.freq)
#lastly calculate TF-IDF
word.raw.tfidf <- apply(word.raw.dfm, 2,tf.idf,idf=word.raw.idf)
dim(word.raw.tfidf)
v <- sort(rowSums(word.raw.tfidf),decreasing = T)
d <- data.frame(word=names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.55, 
          colors=brewer.pal(8, "Dark2"),random.color = T)
####################### Emotion mining #########################
my_example_text <- readLines(file.choose())#taking our txt file of interstellar IMDB

s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)
afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

## To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)



