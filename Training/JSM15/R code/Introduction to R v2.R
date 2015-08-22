#This will demonstrate the basics of natural language processing
#setwd("C:\\Users\\James\\Documents\\Text Mining\\TM for JSM\\data sets jsm")
#load("NLP.RData")
#From RStudio File>open>...NLP.RData
nlp
#We will be using many packages for text mining
#First change working directory under Session to where the binaries a located
textpacks<-c("tm","SnowballC","wordcloud","Matrix","irlba","rpart","DT","tm.plugins.tags")
install.packages(textpacks,dependencies=TRUE)
leftovers<-c("Rcampdf","Rgraphviz","Rpoppler","tm.lexicon.GeneralInquirer")
install.packages(leftovers,repos="http://datacube.wu.ac.at/",type="source")

library(tm)
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
jsm<-Corpus(VectorSource(nlp$Text))
#we may inspect the contents of, e.g., the first document of the corpus
inspect(jsm[1])
as.character(jsm[1])
#Process the text within each document
# remove extra whitespace the tm_map(corpus,function) command simply applies
#'function' to each document of 'corpus'
jsm <- tm_map(jsm, stripWhitespace)

# remove punctuation
jsm<- tm_map(jsm, removePunctuation)

# we can check progress so far on how the TermDocumentMatrix is coming along
inspect(TermDocumentMatrix(jsm))

# convert all characters to lower case
jsm<- tm_map(jsm, tolower)
#Make sure the documents are formatted correctly after applying tolower
#This used to be done automatically in older versions of tm
#Results in an error that the DTM is not TRUE if leave this out
#New tm package automatically converts to lower
jsm<- tm_map(jsm, PlainTextDocument)

#take a look at the common stopwords
stopwords("english")
#remove common 'stopwords'
jsm<- tm_map(jsm, removeWords, stopwords("english"))
View(as.character(jsm[1]))

#stem the document
jsm<- tm_map(jsm, stemDocument, "english")
inspect(TermDocumentMatrix(jsm))

#create a term-document matrix
jsmdtm_tdm_tf <- TermDocumentMatrix(jsm, control = list(weighting = weightTf,
    tolower = FALSE))
    
# remove terms with non UTF-8 charachters
jsmdtm_tdm_tf <- jsmdtm_tdm_tf[!(is.na(iconv(rownames(jsmdtm_tdm_tf),
    to = "UTF-8"))), ]
colnames(jsmdtm_tdm_tf) <- 1:ncol(jsmdtm_tdm_tf)
inspect(jsmdtm_tdm_tf)

#a binary term-document matrix
jsmdtm_tdm_bin <- weightBin(jsmdtm_tdm_tf)
jsmdtm_bin <- as.DocumentTermMatrix(jsmdtm_tdm_bin)
freq_indx <- colSums(dtm.to.Matrix(jsmdtm_bin)) >= 1
jsmdtm_tdm_tf <- jsmdtm_tdm_tf[freq_indx, ]

#convert TDM to DTM, then apply selected weighting
jsmdtm_tf <- as.DocumentTermMatrix(jsmdtm_tdm_tf)

#Control the weighting scheme
weighting_r <- 1 #change to 2 will generate binary table
# 1 for tf-idf
# 2 for Term Frequency
# 3 for Binary
# 4 for Ternary
# 5 for log

if (weighting_r == 2) {
    jsmdtm <- jsmdtm_tf
} else if (weighting_r == 1) {
    jsmdtm <- weightTfIdf(jsmdtm_tdm_tf, normalize = FALSE)
    jsmdtm <- as.DocumentTermMatrix(jsmdtm)
} else if (weighting_r == 3) {
    jsmdtm <- weightBin(jsmdtm_tdm_tf)
    jsmdtm <- as.DocumentTermMatrix(jsmdtm)
} else if (weighting_r == 4) {
    jsmdtm <- jsmdtm_tf
    jsmdtm$v <- pmin(jsmdtm$v, 2)
} else if (weighting_r == 5) {
    jsmdtm <- jsmdtm_tf
    jsmdtm$v <- log(jsmdtm$v + 1, base = 2)
}
rownames(jsmdtm) <- 1:nrow(jsmdtm)
#print information about the DTM
print(jsmdtm)

#print the DTM
printSpMatrix(dtm.to.Matrix(jsmdtm),col.names=TRUE)
View(as.matrix(jsmdtm))
