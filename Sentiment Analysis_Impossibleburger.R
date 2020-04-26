#PROMPT:
#Extract 1200 tweets from twitter related to impossible burger and developed a network and sentiment analysis of the tweets. 


install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")

api_key = "QwmRT2YctsnkaNTVgoUxoBWmn"
api_secret = "LQDHMtD2CQuWxF1kIo2f6IyQ3VZ5LxGALaWB8qxq1x1r00h4GJ"
access_token = "2363172462-pUfSaRGHv8kgVMo8rQXP8SdG2tdyyjZVKqfI1cS"
access_token_secret = "YlfyaoM0cZcNFnCFIAIld03cvj3b8OSnyKsXkPGqatdSR"
  
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

terms = c("impossible burger", "impossibleburger")
tweets = searchTwitter(terms, n=1200, lang = "en")
length(tweets)

impossibleburger = twListToDF(tweets)
write.csv(impossibleburger, file = '~/Desktop/impossibleburger.csv', row.names = F)

str(impossibleburger)

impossibleburger = read.csv("impossibleburger.csv")

#clean the text of special characters such as symbols and emoticons
impossibleburger$text <- sapply(impossibleburger$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#Building Corpus
install.packages(c("NLP", "tm"))
library(tm)
library(NLP)

corpus <-iconv(impossibleburger$text, to='utf-8-mac') #need only the first col text from file
corpus <- Corpus(VectorSource(corpus)) #corpus is a collection of texts
inspect(corpus[1:5]) #inspect the first five tweets

#convert data to lower case for analysis
corpus <-tm_map(corpus, tolower) #convert all alphabet to lower case
inspect(corpus[1:5]) #inspect the first five tweets

#remove punctuations
corpus <-tm_map(corpus, removePunctuation)
inspect(corpus[1:5]) #inspect the first five tweets

#remove numbers
corpus <-tm_map(corpus, removeNumbers)
inspect(corpus[1:5]) #inspect the first five tweets

#select stopwords(english) to see what words are removed
cleanset <-tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])
stopwords("english")

#remove URLs (https://etc.); make use of function http
removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
cleanset <-tm_map(cleanset, content_transformer(removeURL))

#tweets were pulled using impossible burger so we can clean it from the text
cleanset <-tm_map(cleanset, removeWords, c('impossible burger', "burger", "impossible"))
inspect(cleanset[1:5])

#remove white spaces
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#lets now provide some structure to tweets by creating a matrix of rows/columns 
#Create term document matrix (tdm)
tdm <- TermDocumentMatrix(cleanset)
tdm

tdm <- as.matrix(tdm)
tdm[1:10, 1:20] #look at first 10 rows/terms and 20 tweets

#Sum the rows (how many times a term appears), Too many terms so we can
#create a subset of w where row sum is >30
install.packages("RColorBrewer")
library("RColorBrewer")
w <- rowSums(tdm)
w <- subset(w, w>=30) #run "w" to see which words appear how many times
barplot(w, las = 2, col=brewer.pal(n = 30, name = "Spectral"), 
        main="Most Frequent Words in Impossible Burger Tweets",
        ylab="Frequency",
        cex.axis = 1) 

# Word Cloud
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)
w <- sort(rowSums(tdm),
          decreasing=TRUE)
set.seed(9999)
wordcloud(words = names(w), freq=w, random.order =FALSE, max.words = 200)


wordcloud(words = names(w), freq=w,random.order =FALSE, max.words = 200, min.freq = 5, colors = brewer.pal(8, 'Dark2'), scale = c(3, 0.2), rot.per = .3) 


#load packages
install.packages("syuzhet")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)

#reading Files
#take the initial apple tweet file (1000 obs and 16 vars for this)
#take the first column, text and put it into tweets dataframe
tweets <- iconv(impossibleburger$text, to="utf-8-mac")

#obtain sentiment scores for each 1000 tweets; nrc_sentiment dictionary is called to calculate presence of eight emotions in their text file
s <-get_nrc_sentiment(tweets)
head(s)
tail(s)

#lets sum the column scores across tweets for the plot
barplot(colSums(s), las = 2, ylab = 'Total Count', main ='Sentiment Scores for Impossible Burger Tweets')

library("igraph")
g <- graph.adjacency(termM, weighted=T, mode ='undirected') #convert it into graph, no direction for edges


#remove terms that have loops (going to self)
g <- simplify(g)
#set labels and degrees of Vertices (V), each word is a vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g) 


#Histogram of node degree, use 100 bars (too many words), label of y and x axis
hist(V(g)$degree,
     breaks=100,
     col='green',
     main ='histogram of node degree',
     ylab ='frequency',
     xlab='degree of vertices')

set.seed(999)
plot(g)


tdm <- tdm[rowSums(tdm)>30,]
#include only terms having frequency more than 30 and rerun plot
#play with options such as size of vertex, distance of labels, etc. then have labels of vertex


comm <- cluster_edge_betweenness(g)
plot(comm, g)
#Plot based on edge betweenness for different dense areas we have different clusters

prop <-cluster_label_prop(g)
plot(prop, g)
#label propogation

#greedy algorithm for clustering
greed <-cluster_fast_greedy(as.undirected(g)) 
plot(greed, as.undirected(g))



egam <- (log(E(g)$weight) + 0.2)/ max(log(E(g)$weight) + 0.2)
E(g)$color <- rgb(0.5, 0.5, 0, egam)
E(g)$width <- egam
g2 <- delete.vertices(g, V(g)[degree(g)<100]) plot(g2, vertex.label.cex =0.90, ertex.label.color ='black')



#Delete edges - delete some edges to make the network better
#(delete edges less than 2) and (delete vertices less than 120)
E(g)$color <- rgb(0.5, 0.5, 0, egam)
E(g)$width <- egam
g3<- delete.edges(g, E(g)$weight<- 2)
g3 <- delete.vertices(g3, V(g3)[degree(g3)<120])
plot(g3)
