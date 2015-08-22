#NTSB text mining analysis
#This program will read the NTSB reports as individual text files from
#a folder
#install new packages if needed
#textpacks<-c("tm","SnowballC","wordcloud","Matrix","irlba","rpart","DT","tm.plugins.tags")
#install.packages(textpacks,dependencies=TRUE)
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
#result is now a flat file with the text in the second column and text file number in first
#check out the first 5 rows of text
result[1:5,2]
#alternatively, first 6 rows
head(result)
View(result)

###############################################################
#We could have loaded the entire previously saved data set
#setwd("C:\\Users\\James\\Documents\\Text Mining\\TM for JSM\\R\\R Programs for TM")
#load("NTSB.RData")
###############################################################
#alternatively we could read the Excel NTSB.csv file
#NTSBdata<-read.csv(file="C:\Users\James\Documents\Text Mining\TM for JSM\R\R Programs for TM\\NTSB.csv") 
###############################################################

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
library(DT)

#create a "corpus", a single object which contains all of the reports as
#elements
IC<-Corpus(VectorSource(result[,2]))
#we may inspect the contents of, e.g., the first document of the corpus
inspect(IC[1])
as.character(IC[1])

#Process the text within each document
# remove extra whitespace the tm_map(corpus,function) command simply applies
#'function' to each document of 'corpus'
IC <- tm_map(IC, stripWhitespace)
# remove punctuation
IC <- tm_map(IC, removePunctuation)
# convert all characters to lower case
IC <- tm_map(IC, tolower)
#Make sure the documents are formatted correctly after applying tolower
#This used to be done automatically in older versions of tm
IC <- tm_map(IC, PlainTextDocument)
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
rownames(sent.analysis.mat) <- 1:nrow(sent.analysis.mat)
#Results of the sentiment analysis
View(sent.analysis.mat)

#remove common 'stopwords'
IC <- tm_map(IC, removeWords, stopwords("english"))

#stem the document
IC <- tm_map(IC, stemDocument, "english")

#create a term-document matrix
ICdtm_tdm_tf <- TermDocumentMatrix(IC, control = list(weighting = weightTf,
    tolower = FALSE))
    
# remove terms with non UTF-8 charachters
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
weighting_r <- 3
# 1 for tf-idf
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
rownames(ICdtm) <- 1:nrow(ICdtm)
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
#we use pallete Dark2, try Paired, Greens (or any other color), Accent, Set1, Set2, Set3
#Cynthia Brewer is from Penn State
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
View(dictionary_res_r[order(as.numeric(dictionary_res_r[,2])),])

#SVD document scores for clustering
dtm.projection<-as.matrix(dtm.to.Matrix(ICdtm) %*% svd.v)
dtm.projection <- t(apply(dtm.projection, 1, function(row) row/sqrt(sum(row^2))))
dtm.projection[is.na(dtm.projection)]<-0
svd.names <- paste("SVD", 1:ncol(dtm.projection), sep = "")
colnames(dtm.projection) <- svd.names
rownames(dtm.projection) <- result[,1]

#SVD representation of first five documents
dtm.projection[1:5,]

#SVD term scores for clustering
term.scores<- t(as.matrix(diag(svd.d)%*%t(svd.v)))
rownames(term.scores)<-words
#The eigenvectors from the V matrix are interpretable as each one could
#represent a topic or theme. Use the DT library function datatable
#to rapidly sort and view words associated with each of the s topics
#it may take a while to come up as html if use all s, so just do a subset
datatable(term.scores[,1:30])
#term2.scores<- t(as.matrix(varimax(diag(svd.d)%*%t(svd.v))))
#datatable(term2.scores[,1:30])
#create a dense version of the DTM for CART
dtm.dense <- as.matrix(ICdtm)
datatable(dtm.dense[1:10,])

#cluster the documents
hc <- hclust(dist(dtm.projection), "ward.D2")
#plot the joining distances
x11()
plot(rev(hc$height))
doc.cluster<-cbind(result,cutree(hc,k=150))
colnames(doc.cluster)[3]<-"Cluster"
doc.cluster$Cluster <- factor(doc.cluster$Cluster)
print(doc.cluster[1,])

#print the documents in Cluster 1
print(doc.cluster[doc.cluster[,3]==1,][,2])


#cluster the terms
worddist<-dist(term.scores)
hc.term <- hclust(worddist, "ward.D2")
#plot the joining distances
x11()
plot(rev(hc.term$height))
term.cluster<-as.data.frame(cbind(words,as.numeric(cutree(hc.term,k=150))))
term.cluster[,2]<-as.numeric(term.cluster[,2])
names(term.cluster)[2]<-"Cluster"
term.cluster <- term.cluster[order(term.cluster[,2]),]
View(term.cluster)
#now we can check out which words are closest to selected words of interest
#looking at whole matrix takes about 2 mins to load and has warnings, look at subset
#sort on stall, spin, stallspin, student, suicide,takeoff
worddist<-as.matrix(worddist)
datatable(worddist[,1100:1200])

#read in fatal indicators from NTSB_fatal.csv
fatal.csv <- read.csv(file=file.choose())
tree.data <- cbind(as.data.frame(dtm.dense),fatal.indicator=fatal.csv[,2])
head(tree.data)

#CART on the terms for fatal.indicator
fatal.tree <- rpart(fatal.indicator~.,data=tree.data)
opar<-par(xpd=TRUE)
plot(fatal.tree)
text(fatal.tree)
par(opar)
print(fatal.tree)

#These words are most strongly associated with fatal/non-fatal
fatal.tree$variable.importance

#tabulate clusters by fatal.indicator
tab <- table(doc.cluster$Cluster,fatal.csv[,2])
percent.fatal <- data.frame(cbind(cluster=1:nlevels(doc.cluster$Cluster),percent.fatal=tab[,2]/rowSums(tab),rowSums(tab)))
print(sorted.percent<-percent.fatal[order(percent.fatal$percent.fatal,decreasing=TRUE),])

#print the three most fatal clusters
print(doc.cluster[doc.cluster[,3]==sorted.percent[1,1],][,2])
print(doc.cluster[doc.cluster[,3]==sorted.percent[2,1],][,2])
print(doc.cluster[doc.cluster[,3]==sorted.percent[3,1],][,2])

#logistic regression for fatal.indicator using SVD vectors
logistic.data <- cbind(data.frame(dtm.projection),fatal.indicator=fatal.csv[,2])
#By default, R models the second listed level (YES, in this case)
#as the postive class
levels(logistic.data$fatal.indicator)
#There is significant information in the SVD vectors relating to the
#fatal indicators
summary(glm(fatal.indicator~.,data=logistic.data,family=binomial()))

#CART on Fatal versus the SVDs
ntsbtree<-rpart(fatal.indicator~.,data=logistic.data)
ntsbtree
ntsbtree$variable.importance
plot(ntsbtree)
text(ntsbtree)
#find out what words are loaded on those SVDs 
datatable(term.scores)
