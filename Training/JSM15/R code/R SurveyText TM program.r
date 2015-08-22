#######
#IMPORT FOLDER OF TXT FILES
#######
#least favorite stop
dir.name <- choose.dir(caption = "Select a folder of .txt files")
library(tm)


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
datatable(result)

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
#Put in correct format
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

#stem the document
IC <- tm_map(IC, stemDocument, "english")

#create a term-document matrix
ICdtm_tdm_tf <- TermDocumentMatrix(IC, control = list(weighting = weightTf,
    tolower = FALSE))

# remove non UTF-8 charachters
ICdtm_tdm_tf <- ICdtm_tdm_tf[!(is.na(iconv(rownames(ICdtm_tdm_tf),
    to = "UTF-8"))), ]
rownames(ICdtm_tdm_tf)<-eval(parse(text=gsub("\\", "", deparse(rownames(ICdtm_tdm_tf)), fixed=TRUE)))

#a binary term-document matrix
ICdtm_tdm_bin <- weightBin(ICdtm_tdm_tf)
ICdtm_bin <- as.DocumentTermMatrix(ICdtm_tdm_bin)
freq_indx <- colSums(dtm.to.Matrix(ICdtm_bin)) >= 3
ICdtm_tdm_tf <- ICdtm_tdm_tf[freq_indx, ]

#remove generic stopwords and re-create binary DTM
ICdtm_tdm_tf <- ICdtm_tdm_tf[!(rownames(ICdtm_tdm_tf)%in%stemDocument(removePunctuation(stopwords("english")))), ]
ICdtm_tdm_bin <- weightBin(ICdtm_tdm_tf)
ICdtm_bin <- as.DocumentTermMatrix(ICdtm_tdm_bin)

#convert TDM to DTM, then apply selected weighting
ICdtm_tf <- as.DocumentTermMatrix(ICdtm_tdm_tf)

#can find binomial associations in the DTM with a chosen word
findAssocs(ICdtm_tf,c("park","dirti","bad"),corlimit=.2)

#Control the weighting scheme
weighting_r <- 3
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
nU <- nV <- 5

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

suppressWarnings(wordcloud(word.mat[,1],as.numeric(word.mat[,2]),max.words=100,min.freq=0,random.order=FALSE,colors=color.list,scale=c(6,1)))

#print the word counts
dense.names <- as.character(rownames(ICdtm_tdm_tf))
words <- dense.names
counts <- as.numeric(colSums(dtm.to.Matrix(ICdtm_tf)))
dictionary_res_r <- as.data.frame(cbind(words, counts))
dictionary_res_r[,2]<-as.numeric(as.character(dictionary_res_r[,2]))
View(dictionary_res_r[order(as.numeric(dictionary_res_r[,2])),])

