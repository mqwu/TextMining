#install new packages
textpack3<-c("streamR","twitter815")
install.packages(textpack3,dependencies=TRUE)
##########################################################################
#Options
#Random Sample of All Tweets?
#If true, any search phrase entered below will be ignored
random_tweets <- FALSE
#How long should the mining operation last (seconds)?
timeout <- 30
#Enter search phrases, separated by commas. Spaces act like logical AND’s, 
#commas act like logical OR’s. Quotation marks are ignored, while other 
#punctuation is appended to the adjacent characters. The order of terms in a 
#phrase (words separated by spaces) is irrelevant.
query<-"today"
#max number of tweets to collect
max.tweets<-1e6
##########################################################################
library(streamR)
library(twitter815)
data(twitCred)
if(file.exists("tweets_R.json")) file.remove("tweets_R.json")
if(!random_tweets){ filterStream( file="tweets_R.json",track=query, timeout=timeout, oauth=twitCred ,tweets=max.tweets )
}else{
	sampleStream( file="tweets_R.json", timeout=timeout, oauth=twitCred, tweets=max.tweets )
}
df.tweets<-parseTweets("tweets_R.json",simplify=FALSE)
#Output writen to a csv file in your home folder
write.csv(df.tweets,file=paste("twitter_output_",gsub(":",".",date()),".csv",sep=""))


