rm(list=ls(all=TRUE))
#set wd
library("rJava")
library("rWeka")
library("rWekajars")
library("Snowball")
library("SnowballC")
library("stringr")
library("tm")

#read file
jul8.df <- read.csv("jul8.csv", header=TRUE, sep=",")
summary(jul8.df)
colnames(jul8.df)

#Hashtag Analysis
jul8hash <- jul8.df[ , 5 ]
#turn it into a df, add a count figure, and adjust column names
jul8hash.df <- as.data.frame(jul8hash, row.names = NULL)
jul8hash.df$n <- 1
colnames(jul8hash.df)
colnames(jul8hash.df)[1] <- 'word'
#none of these approaches worked to get rid of rows with no value
jul8hash.df <- jul8hash.df[!is.na(jul8hash.df$word), ]
is.na(jul8hash.df$word)
jul8hash.df[!complete.cases(jul8hash.df), 1 ]
jul8hash.df <- jul8hash.df[jul8hash.df$word != 0, ]

#Add up number of instances by work, arrange in decending order
#load plyr
jul8hashTtl.df <- ddply(jul8hash.df , "word", summarise, total = sum(n) )
jul8hashTtl.df <- arrange(jul8hashTtl.df, desc(total))
head(jul8hashTtl.df)
jul8hashTtl.df[1:50, ]
jul8hashTtl.df <- jul8hashTtl.df[jul8hashTtl.df$total > 2, ]

#repeat for 9th and 10th
jul10.df <- read.csv("jul10.csv", header=TRUE, sep=",")
jul10hash <- jul10.df[ , 5 ]
jul10hash.df <- as.data.frame(jul10hash, row.names = NULL)
jul10hash.df$n <- 1
colnames(jul10hash.df)[1] <- 'word'
colnames(jul10hash.df)
jul10hashTtl.df <- ddply(jul10hash.df , "word", summarise, total = sum(n) )
head(jul10hashTtl.df)
jul10hashTtl.df <- arrange(jul10hashTtl.df, desc(total))
jul10hashTtl.df <- jul10hashTtl.df[jul10hashTtl.df$total >2, ]

Check <- jul8.df[jul8.df$hashtag == "forex", ]

##RT Analysis
head(jul8.df)
#This returned zero results
RT10 <- jul10.df[jul10.df$retweeted == "true", ]
#isolate text and turn into corpus
jul8txt <- jul8.df[ , 1]
jul8txt <- as.data.frame(jul8txt, row.names = NULL)
jul8.corpus <- Corpus(DataframeSource(data.frame(jul8txt)))
atRT8 <- tm_filter(jul8.corpus, pattern = "@RT")
RT8 <- tm_filter(jul8.corpus, pattern = "RT")
rt8 <- tm_filter(jul8.corpus, pattern = "rt")
RT8 <- as.data.frame(RT8)
write.csv(RT8, "RT8.csv", row.names=TRUE)

#opened RT8.csv, transposed
newRT8.df <- read.csv("RT8.csv", row.names = NULL)
RT8.corpus <- Corpus(DataframeSource(data.frame(newRT8.df))
RT8.corpus <- tm_map(RT8.corpus, stripWhitespace)
RT8.corpus <- tm_map(RT8.corpus, function(x) removeWords(x, stopwords("english")))           
tdmRT8 <- TermDocumentMatrix(RT8.corpus)
tdmRT8.m <- as.matrix(tdmRT8, row.names=TRUE)
v1<- sort(rowSums(tdmRT8.m),decreasing=TRUE)
summary(v1)
head(v1)
write.csv(v1, "smallrtFreqTerms.csv", row.names=TRUE)
  
newsmrt8.df <- read.csv("smallrt8.csv", row.names = NULL)
smrt.corpus <- Corpus(DataframeSource(data.frame(newsmrt8.df)))                                   
smrt.corpus <- tm_map(smrt.corpus, stripWhitespace)
smrt.corpus <- tm_map(smrt.corpus, function(x) removeWords(x, stopwords("english")))           
tdmrt8 <- TermDocumentMatrix(smrt.corpus)
#list of terms appearing more than 10x
findFreqTerms(tdmrt8, 10)                  
#find Frequent terms by count
tdmrt8.m <- as.matrix(tdmrt8, row.names=TRUE)                    
v1<- sort(rowSums(tdmrt8.m),decreasing=TRUE)
summary(v1)       
write.csv(v1, "smallrt8FreqTerms.csv", row.names=TRUE)
                     
#Analysis of full jul8.corpus  (obj already exists)
jul8.corpus <- tm_map(jul8.corpus, stripWhitespace)
jul8.corpus <- tm_map(jul8.corpus, tolower)
jul8.corpus <- tm_map(jul8.corpus, removePunctuation)   #optional
jul8.corpus <- tm_map(jul8.corpus, function(x) removeWords(x, stopwords("english")))
       #pause
jul8.corpus <- tm_map(jul8.corpus, stemDocument)
tdmjul8 <- TermDocumentMatrix(jul8.corpus)
tdmjul8.m <- as.matrix(tdmjul8, row.names=TRUE)
v1<- sort(rowSums(tdmjul8.m),decreasing=TRUE)
summary(v1)       
write.csv(v1, "jul8FreqTerms.csv", row.names=TRUE) 
hjul8 <- hclust(dist(tdmjul8), method="ward")       
      
