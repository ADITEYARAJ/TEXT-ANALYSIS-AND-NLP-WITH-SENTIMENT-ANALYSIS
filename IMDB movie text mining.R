update.packages("tidyverse")
update.packages("rlang")
install.packages("quanteda")
install.packages("readtext")
install.packages("irlba")
install.packages("rlang")
install.packages("devtools")
install.packages("quanteda.textmodels")
install.packages("topicmodels")
devtools::install_github("quanteda/quanteda.corpora")
install.packages("syuzhet")
library("syuzhet")
library(rlang)
library(caret)
library(quanteda)
library(readtext)
library(irlba)
library(wordcloud)
library(irlba)
library(topicmodels)
library(rvest)
library(XML)
library(magrittr)
############# IMDB reviews Extraction ################

interstellar <- NULL
url.interstellar <- "https://www.imdb.com/title/tt0816692/reviews?ref_=tt_urv"
for (i in 1:50){
  murl <- read_html(as.character(paste(url.interstellar,i,sep="")))
  rev1 <- murl %>%
    html_nodes(".review-container") %>%
    html_text()
  inter_reviews <- c(interstellar,rev1)
}
write.table(inter_reviews,"interstellar.reviews.txt",row.name=F)
getwd()
inter.raw <- tokens(inter_reviews,what="word",remove_number=T,remove_punct = T,remove_symbols = T,split_hyphens=T)
inter.raw <- tokens_select(inter.raw,c("Spoilers","Permalink","helpful"," Sign","vote","review","November"," April","found"),selection = "remove")
#Lower case the tokens
inter.raw <- tokens_tolower(inter.raw)
#to remove stopwords
inter.raw <- tokens_select(inter.raw,stopwords(),selection = "remove")
inter.raw <- tokens_wordstem(inter.raw,language = "english")
inter.raw <- tokens_ngrams(inter.raw,n=1:2)
inter.raw.dfm <- dfm(inter.raw)
inter.raw.matrix <- as.matrix(inter.raw.dfm)                           
inter.raw.matrix1 <- as.data.frame(inter.raw.matrix)                           
View(inter.raw.matrix1)
s <- 0
for (i in 1:nrow(inter.raw.matrix)){
  if(sum(inter.raw.matrix1[i,])==0){
    inter.raw.matrix2 <- inter.raw.matrix1[-i,]
    s <- s+1
  }
  
}
print(s)
inter.raw.matrix3<- inter.raw.matrix2[which(rowSums(inter.raw.matrix2) > 0),]
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
inter.raw.dfm <- apply(inter.raw.matrix3,1,term.frequency)
dim(inter.raw.dfm)
#second,calculate IDF
inter.raw.idf <- apply(inter.raw.matrix3, 2,inverse.doc.freq)
#lastly calculate TF-IDF
inter.raw.tfidf <- apply(inter.raw.dfm, 2,tf.idf,idf=inter.raw.idf)
dim(inter.raw.tfidf)
View(inter.raw.tfidf)
str(inter.raw.tfidf)
v <- sort(rowSums(inter.raw.tfidf),decreasing = T)
d <- data.frame(word=names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.55, 
          colors=brewer.pal(8, "Dark2"),random.color = T)
####################### Emotion mining ##############################
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
# percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
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

