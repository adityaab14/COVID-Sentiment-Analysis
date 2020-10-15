#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(openssl)
library(httpuv)
library(base64enc)
library(tmap)
library(tm)
library(syuzhet)
library(wordcloud)
library(SnowballC)
library(stringi)
library(topicmodels)


download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
#options(httr_oauth_cache=T)
devtools::install_version("httr", version="1.0.0", repos="http://cran.us.r-project.org")
api_key = "8zaEuPipuNl4T9GLBHc3qnL2z" # your api_key
api_secret = "fpYvLJT7L0q6TPQ7KOI94JSEgifHV5VHqHWX94SciaGNOCLXCz" # your api_secret 
access_token = "2599960604-dK8qZvGx4Y1MJPwkp5YAwncfBFKFqDpcNe0O9pD" # your access_token 
access_token_secret = "ifC7bA0OQYo1s8AOmF7hFMMZdT8Up21tAkWxEdwJKNCgv" # your access_token_sceret 
credential<-OAuthFactory$new(consumerKey=api_key,
                             consumerSecret=api_secret,
                             requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL="https://api.twitter.com/oauth/access_token",
                             authURL="https://api.twitter.com/oauth/authorize")

credential$handshake()

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

tweets_c <- searchTwitter("#Covid_19", n=1000,lang = "en")

covid_tweets <- twListToDF(tweets_c)
covid_text<- covid_tweets$text
#convert all text to lower case
covid_text<- tolower(covid_text)
# Replace blank space (“rt”)
covid_text <- gsub("rt", "", covid_text)
# Replace @UserName
covid_text <- gsub("@\\w+", "", covid_text)
# Remove punctuation
covid_text <- gsub("[[:punct:]]", "", covid_text)
# Remove links
covid_text <- gsub("http\\w+", "", covid_text)
# Remove tabs
covid_text <- gsub("[ |\t]{2,}", "", covid_text)
# Remove blank spaces at the beginning
covid_text <- gsub("^ ", "", covid_text)
# Remove blank spaces at the end
covid_text <- gsub(" $", "", covid_text)
#generate wordcloud
wordcloud(covid_text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)
#getting emotions using in-built function
mysentiment_covid<-get_nrc_sentiment((covid_text))
#calculationg total score for each sentiment
Sentimentscores_covid<-data.frame(colSums(mysentiment_covid[,]))
names(Sentimentscores_covid)<-"Score"
Sentimentscores_covid<-cbind("sentiment"=rownames(Sentimentscores_covid),Sentimentscores_covid)
rownames(Sentimentscores_covid)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores_covid,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Corona Virus")



