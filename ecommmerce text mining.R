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

################# Amazon Reviews #############################
aurl <-  "https://www.amazon.in/Moto-G5-GB-Fine-Gold/product-reviews/B01N7JUH7P/ref=cm_cr_getr_d_paging_btm_3?showViewpoints=1&pageNumber=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".a-size-base") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"Moto.txt",row.name=F)
getwd()
moto.raw <- tokens(amazon_reviews,what="word",remove_number=T,remove_punct = T,remove_symbols = T,split_hyphens=T)
#Lower case the tokens
moto.raw <- tokens_tolower(moto.raw)
#to remove stopwords
moto.raw <- tokens_select(moto.raw,stopwords(),selection = "remove")
moto.raw <- tokens_wordstem(moto.raw,language = "english")
moto.raw <- tokens_select(moto.raw,c("star","abus","sort","stars","customer","ratings","moto","motorola","review","reviewed","motorola","Showing","bymotorola","india","november","january","shortby","comment"),selection = "remove",padding = T)
moto.raw <- tokens_ngrams(moto.raw,n=1:2)
moto.raw.dfm <- dfm(moto.raw)
moto.raw.matrix <- as.matrix(moto.raw.dfm)
moto.raw.matrix1 <- as.data.frame(moto.raw.matrix)
View(moto.raw.matrix1)
s <- 0
for (i in 1:nrow(moto.raw.matrix)){
     if(sum(moto.raw.matrix1[i,])==0){
         moto.raw.matrix2 <- moto.raw.matrix1[-i,]
        s <- s+1
       }
    
     }
print(s)
View(moto.raw.matrix2)
moto.raw.matrix3<- moto.raw.matrix2[which(rowSums(moto.raw.matrix2) > 0),]
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
moto.raw.dfm <- apply(moto.raw.matrix3,1,term.frequency)
dim(moto.raw.dfm)
View(moto.raw.dfm[1:20,1:500])
#second,calculate IDF
moto.raw.idf <- apply(moto.raw.matrix3, 2,inverse.doc.freq)
#lastly calculate TF-IDF
moto.raw.tfidf <- apply(moto.raw.dfm, 2,tf.idf,idf=moto.raw.idf)
dim(moto.raw.tfidf)
View(moto.raw.tfidf)
str(moto.raw.tfidf)
v <- sort(rowSums(moto.raw.tfidf[-1,]),decreasing = T)
d <- data.frame(word=names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=5000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
moto.raw.tfidf <- as.data.frame(moto.raw.tfidf)
#perform LSA and SVD for sellection 300 important words from 700 words
moto.irlba <- irlba(t(moto.raw.tfidf),nv=300,maxit = 600)
View(moto.irlba$v)
####################### Emotion mining ##############################
my_example_text <- readLines(file.choose())#taking our txt file of moto amazon review

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



