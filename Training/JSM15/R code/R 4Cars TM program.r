#######
#IMPORT FOLDER OF TXT FILES
#######
library(tm)
dir.name <- choose.dir(caption = "Select a folder of .txt files")
file.names <- list.files(dir.name)
file.types <- numeric(length(file.names))
file.types[grep("\\.(txt)$", file.names, )] <- 1
file.types[grep("\\.(htm)$|\\.(html)$", file.names, )] <- 2
file.types <- as.numeric(file.types)
file.types2 <- file.types[which(file.types != 0)]
file.names <- file.names[which(file.types != 0)]
corp <- Corpus(DirSource(directory = dir.name, pattern = "\\.(txt)$"))
file.names <- file.names[file.types2 == 1]
corp <- tm_map(corp, stripWhitespace)
result <- c(NULL)
for (i in 1:length(corp)) result <- c(result, paste(as.character(corp[[i]]), collapse = ""))
result <- as.data.frame(result)
result <- cbind(file.names, result)
names(result) <- c("file.name", "text")
result[, 2] <- as.character(result[, 2])
#result is now a flat file with the text in the second column

#dtm.to.Matrix converts a sparse DTM to a sparse matrix without
#creating a dense matrix in the process
dtm.to.Matrix<- function(dtm){
    m <- sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v, dims = c(dtm$nrow, dtm$ncol))
    rownames(m)<-dtm$dimnames$Docs
    colnames(m)<-dtm$dimnames$Terms
    return(m)
}

#Load the required packages
library(irlba)
library(SnowballC)
library(Matrix)
library(tm.plugin.tags)
library(wordcloud)
library(rpart)

#create a "corpus", a single object which contains all of the reports as
#elements
IC<-Corpus(VectorSource(result[,2]))
#we may inspect the contents of, e.g., the first document of the corpus
inspect(IC[1])

#Process the text within each document
# remove extra whitespace the tm_map(corpus,function) command simply applies
#'function' to each document of 'corpus'
IC <- tm_map(IC, stripWhitespace)
# remove punctuation
IC <- tm_map(IC, removePunctuation)
# convert all characters to lower case
IC <- tm_map(IC, tolower)
# remove numbers
IC <- tm_map(IC, removeNumbers)

#sentiment analysis
#list of positive words
tm_get_tags("Positiv")
#list of negative words
tm_get_tags("Negativ")
num.positive<-tm_term_score(TermDocumentMatrix(IC),tm_get_tags("Positiv"))
negative.words<-tm_get_tags("Negativ")
num.negative<-tm_term_score(TermDocumentMatrix(IC),negative.words)
num.total<-num.positive+num.negative
num.percent.positive<-num.positive/num.total
sent.analysis.mat<-cbind(num.positive,num.negative,num.total,num.percent.positive)
sent.names<-c("Positive","Negative","Sum Positive and Negative","Percent Positive")
colnames(sent.analysis.mat)<-sent.names
#Results of the sentiment analysis
sent.analysis.mat

#remove common 'stopwords'
IC <- tm_map(IC, removeWords, stopwords("english"))

#stem the document
IC <- tm_map(IC, stemDocument, "english")

#create a term-document matrix
ICdtm_tdm_tf <- TermDocumentMatrix(IC, control = list(weighting = weightTf,
    tolower = FALSE))
    
# remove non UTF-8 charachters
ICdtm_tdm_tf <- ICdtm_tdm_tf[!(is.na(iconv(rownames(ICdtm_tdm_tf),
    to = "UTF-8"))), ]

#a binary term-document matrix
ICdtm_tdm_bin <- weightBin(ICdtm_tdm_tf)
ICdtm_bin <- as.DocumentTermMatrix(ICdtm_tdm_bin)
freq_indx <- colSums(dtm.to.Matrix(ICdtm_bin)) >= 2
ICdtm_tdm_tf <- ICdtm_tdm_tf[freq_indx, ]

#convert TDM to DTM, then apply selected weighting
ICdtm_tf <- as.DocumentTermMatrix(ICdtm_tdm_tf)

#Control the weighting scheme
weighting_r <- 1
# 1 for tf=idf
# 2 for Term Frequency
# 3 for Binary
# 4 for Ternary
# 5 for log

if (weighting_r == 2) {
    ICdtm <- ICdtm_tf
} else if (weighting_r == 1) {
    ICdtm <- weightTfIdf(ICdtm_tdm_tf, normalize = TRUE)
    ICdtm <- as.DocumentTermMatrix(ICdtm)
} else if (weighting_r == 3) {
    ICdtm <- weightBin(ICdtm_tdm_tf)
    ICdtm <- as.DocumentTermMatrix(ICdtm)
} else if (weighting_r == 4) {
    ICdtm <- ICdtm_tf
    ICdtm$v <- pmin(ICdtm$v, 2)
} else if (weighting_r == 5) {
    ICdtm <- ICdtm_tf
    ICdtm$v <- log(ICdtm$v + 1, base = 2)
}
#print information about the DTM
print(ICdtm)

#print the first 20 rows and columns of the DTM
printSpMatrix(dtm.to.Matrix(ICdtm)[1:20,1:20],col.names=TRUE)

#how many SVD vectors should be returned
nU <- nV <- 100

#SVD of the DTM
svd <- irlba(dtm.to.Matrix(ICdtm), nu = nU, nv = nV)
svd.v <- svd$v
svd.d <- svd$d
svd.u <- svd$u

#LSI plot of the first two SVD vectors
plot(t(diag(svd.d[1:2]) %*% t(svd.v[, 1:2])),type="n",xlab="",ylab="")
text(t(diag(svd.d[1:2]) %*% t(svd.v[, 1:2])),colnames(ICdtm), cex = 0.6)

#create a wordcloud
v <- colSums(dtm.to.Matrix(ICdtm))
color.list <- brewer.pal(8,"Dark2")
dense.names<-colnames(ICdtm)
word.mat<-cbind(dense.names,v)
#open new plot window
x11()
suppressWarnings(wordcloud(word.mat[,1],as.numeric(word.mat[,2]),max.words=100,min.freq=0,random.order=FALSE,colors=color.list,scale=c(6,1)))

#print the word counts
dense.names <- as.character(rownames(ICdtm_tdm_tf))
words <- dense.names
counts <- as.numeric(colSums(dtm.to.Matrix(ICdtm_tf)))
dictionary_res_r <- as.data.frame(cbind(words, counts))
dictionary_res_r[,2]<-as.numeric(as.character(dictionary_res_r[,2]))
dictionary_res_r[order(as.numeric(dictionary_res_r[,2])),]

#SVD document scores for clustering
dtm.projection<-as.matrix(dtm.to.Matrix(ICdtm) %*% svd.v)
dtm.projection <- t(apply(dtm.projection, 1, function(row) row/sqrt(sum(row^2))))
dtm.projection[is.na(dtm.projection)]<-0
svd.names <- paste("SVD", 1:ncol(dtm.projection), sep = "")
colnames(dtm.projection) <- svd.names
rownames(dtm.projection) <- result[,1]

#SVD term scores for clustering
term.scores<- t(as.matrix(diag(svd.d)%*%t(svd.v)))
rownames(term.scores)<-words

#create a dense version of the DTM for CART
dtm.dense <- as.matrix(ICdtm)


#cluster the documents
hc <- hclust(dist(dtm.projection), "ward.D2")
#plot the joining distances
x11()
plot(rev(hc$height))
doc.cluster<-cbind(result,cutree(hc,k=50))
colnames(doc.cluster)[3]<-"Cluster"
#doc.cluster <- as.data.frame(doc.cluster[order(doc.cluster[,3]),])
doc.cluster$Cluster <- factor(doc.cluster$Cluster)
print(doc.cluster[1,])

#print the documents in Cluster 1
print(doc.cluster[doc.cluster[,3]==1,][,2])


#cluster the terms
hc.term <- hclust(dist(term.scores), "ward.D2")
#plot the joining distances
x11()
plot(rev(hc.term$height))
term.cluster<-as.data.frame(cbind(words,as.numeric(cutree(hc.term,k=150))))
term.cluster[,2]<-as.numeric(term.cluster[,2])
names(term.cluster)[2]<-"Cluster"
term.cluster <- term.cluster[order(term.cluster[,2]),]
print(term.cluster)

#read in ratings from 4Cars_ratings.csv
target.response.csv <- read.csv(file=file.choose())
target.response.csv <- cbind(data.frame(target.response.csv), Cluster=doc.cluster$Cluster)
tree.data <- cbind(as.data.frame(dtm.dense),response.variable=target.response.csv[,2])

#CART on the terms for the target variable
term.tree <- rpart(response.variable~.,data=tree.data)
print(term.tree)
term.tree$variable.importance

#tabulate clusters by the response variable
mean.response <- aggregate(target.response.csv$Overall.Rating,list(Cluster=target.response.csv$Cluster),mean)
print(sorted.mean<-mean.response[order(mean.response[,2]),])

#print the three clusters with the lowest overall response
print(doc.cluster[doc.cluster[,3]==sorted.mean[1,1],][,2])
print(doc.cluster[doc.cluster[,3]==sorted.mean[2,1],][,2])
print(doc.cluster[doc.cluster[,3]==sorted.mean[3,1],][,2])

#regression for target respone using SVD vectors
regression.data <- cbind(data.frame(dtm.projection),response.variable=target.response.csv[,2])

#There is significant information in the SVD vectors relating to the
#fatal indicators
summary(regression<-lm(response.variable~.,data=regression.data))

#plot predicted vs overall rating
x11()
plot(predict(regression),regression.data$response.variable)
