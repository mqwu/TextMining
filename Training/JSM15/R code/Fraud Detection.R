#initial install of your packages, make sure you've set the working directory
textpack2<-c("ROCR","randomForest","ggplot2","caret")
install.packages(textpack2,dependencies=TRUE)
leftovers<-c("Rcampdf","Rgraphviz","Rpoppler","tm.lexicon.GeneralInquirer")
install.packages(leftovers,repos="http://datacube.wu.ac.at/",type="source")
#data from Practical Text Mining by Miner et al.
#Code furnished by Adsurgo LLC
#Open  
library(ROCR)
library(randomForest)
library(Matrix)
library(irlba)
library(tm)
library(SnowballC)
library(caret)
# A function to convert a document term matrix to a sparse matrix

dtm.to.Matrix<- function(dtm){
    m <- sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v, dims = c(dtm$nrow, dtm$ncol))
    rownames(m)<-dtm$dimnames$Docs
    colnames(m)<-dtm$dimnames$Terms
    return(m)
}


# read in the file Fraud_Detection_Example.csv and
# save it as a data.frame called �insurance�
insurance<-read.csv(file.choose())
# list the names of the variables in the data.frame 
as.matrix(names(insurance))
# view the first claim
insurance[1,]
insurance$Cause[1:3]

# tell R which continuous and categorical variables we will be using
class(insurance$Cause)
insurance$Cause<-as.character(insurance$Cause)
cont.pred<-c(24,25,76)
cat.pred<-c(33,48,51,57,60,69,71,73,74,75,77,78,79)
target<-c(59)    
# tell  R to treat the continuous factors �as.numeric� and the 
# categorical factors �as.factor�

for(i in cont.pred){
insurance[,i]<-as.numeric(insurance[,i])
}
for(i in cat.pred){
insurance[,i]<-as.factor(insurance[,i])
}
insurance[,target]<-as.factor(insurance[,target])
insurance.reduced<-insurance[,c(target,cont.pred,cat.pred)]


# Create training and testing sets. 
# set.seed(0) sets the seed of the random number generator to 0 to 
# ensure that the same partition will be created each time the code is run

set.seed(0)
# train.index is a vector indicating which rows will be in the training set
train.index <- createDataPartition(insurance$Suspicion_Flag, 1,p=.7)[[1]]
train<-insurance.reduced[train.index,]
test<-insurance.reduced[-train.index,]
table(train$Suspicion_Flag)

# fit a logistic regression
# Suspicion_Flag~. tells R to model Suspicion_Flag
# against all of the other variables in the data set insurance.reduced
# type help(formula) into R for more information.
summary(glm(Suspicion_Flag~.,
            data=insurance.reduced,
            family=binomial(link="logit")))



# fit a random forest on the training data with 500 trees
# using a balanced data set with 473 observations from 
# each level of Suspicion_Flag
structured.rf <- randomForest(Suspicion_Flag~.,
      data=train,
      ntree=500,
      sampsize = c(473, 473),
      importance=TRUE,
      na.action=na.roughfix,
      replace=FALSE)
# produce variable importance plot
varImpPlot(structured.rf)
# open a new plot window
windows()
plot(structured.rf,main="Error Rates Random Forest insurance",ylim=c(0,1))
legend("topright", c("0", "1"), text.col=1:6, lty=1:3, col=1:3)
 
# we will now extract information from the Cause column
# create a "corpus", a single object which contains all of the reports as
# elements
IC<-Corpus(VectorSource(insurance$Cause))
#we may inspect the contents of, e.g., the first document of the corpus
inspect(IC[1])
#remove extra whitespace
#the tm_map(corpus,function) command simply applies "function" to each
#document of "corpus"
IC<-tm_map(IC, stripWhitespace)
#remove punctuation
IC<-tm_map(IC, removePunctuation)
#convert all characters to lower case
IC<-tm_map(IC, tolower)
#remove numbers
IC<-tm_map(IC, removeNumbers)
#remove common "stopwords"
IC <- tm_map(IC, removeWords, stopwords("english"))
#stem the document
IC <- tm_map(IC, stemDocument, "english")
IC <- tm_map(IC, PlainTextDocument)
# Construct the document term matrix. 
# The rows contain documents and the columns contain terms
# We use inverse document frequency
#create a term-document matrix
ICdtm<- DocumentTermMatrix(IC,control=list(weighting= weightTfIdf,tolower=FALSE))
#view the first 10x10 block of the document term matrix
inspect(ICdtm[1:10,1:10])
as.matrix(ICdtm)[1:10,1:10]

# Add the DTM to the data set and create
# new training and testing sets including the DTM
# This direct approach is only reasonable for small
# dictionary sizes.
insurance.dtm<-cbind(insurance.reduced,as.matrix(ICdtm))
train.dtm<-insurance.dtm[train.index,]
test.dtm<-insurance.dtm[-train.index,]


dtm.rf <- randomForest(Suspicion_Flag~.,
      data=train.dtm,
      ntree=500,
      sampsize = c(473, 473),
      importance=TRUE,
      na.action=na.roughfix,
      replace=FALSE)
  varImpPlot(dtm.rf)
  plot(dtm.rf,main="Error Rates Random Forest insurance")
 legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
 
svd_plot<-function(DTM){
#accepts a document term matrix from package tm as input
#plots first two SVD row and column vectors
result<-svd(DTM)
UD<-result$u[,1:2]%*%diag(result$d[1:2])
DV<-diag(result$d[1:2])%*%t(result$v[,1:2])
plot(result$d^2/sum(result$d^2),main="Scree Plot",xlab="Component",ylab="Singular Value /% Explained",type="b")
windows()#new window
plot(UD,type="n",xlab="",ylab="")
text(UD, rownames(DTM), cex = 0.6)
windows()#new window
plot(t(DV),type="n",xlab="",ylab="")
text(t(DV), colnames(DTM), cex = 0.6)
}
svd_plot(ICdtm)

# Reduce the 97 column DTM to 15 columns via a 
# Singular Value Deccomposition
svd.v<-irlba(dtm.to.Matrix(ICdtm),nu=0,nv=15)$v
# dtm.projection contains the "best possible" 5 column summary of the DTM 
dtm.projection<-as.matrix(dtm.to.Matrix(ICdtm)%*%svd.v)
# Name the columns SVD1 - SVD15
colnames(dtm.projection)<-paste("SVD",1:ncol(dtm.projection),sep="")

# Append the 15 singular vectors to the structured data
insurance.svd<-cbind(insurance.reduced,dtm.projection)
train.svd<-insurance.svd[train.index,]
test.svd<-insurance.svd[-train.index,]
svd.rf <- randomForest(Suspicion_Flag~.,
      data=train.svd,
      ntree=500,
      sampsize = c(473, 473),
      importance=TRUE,
      na.action=na.roughfix,
      replace=FALSE)
  varImpPlot(svd.rf)
  plot(svd.rf,main="Error Rates Random Forest insurance",ylim=c(0,1))
 legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)                               

#lift chart
# Obtain predictions for the rf model on insurance [test].
predictions <- predict(structured.rf, na.omit(test), type="prob")[,2]
# Deal with any missing values in the target variable by 
# ignoring any training data with missing target values.
no.miss <- na.omit(na.omit(test)$Suspicion_Flag)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL
if (length(miss.list)) {
	pred <- prediction(predictions[-miss.list], no.miss)
} else {
	pred <- prediction(predictions, no.miss)
}
# Convert rate of positive predictions to percentage.
per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100
# Plot the lift chart.
plot(per, col="green", lty=1, xlab="Caseload (%)", add=FALSE,main="Lift")
# Obtain predictions for the rf model on insurance [test].
predictions <- predict(dtm.rf, na.omit(test.dtm), type="prob")[,2]
# Deal with any missing values in the target variable by 
# ignoring any training data with missing target values.
no.miss <- na.omit(na.omit(test.dtm)$Suspicion_Flag)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL
if (length(miss.list)) {
	pred <- prediction(predictions[-miss.list], no.miss)
} else {
	pred <- prediction(predictions, no.miss)
}
# Convert rate of positive predictions to percentage.
per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100
# Plot the lift chart.
plot(per, col="blue", lty=1, xlab="Caseload (%)", add=TRUE)
# Obtain predictions for the rf model on insurance [test].
predictions <- predict(svd.rf, na.omit(test.svd), type="prob")[,2]
# Deal with any missing values in the target variable by 
# ignoring any training data with missing target values.
no.miss <- na.omit(na.omit(test.svd)$Suspicion_Flag)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL
if (length(miss.list)) {
	pred <- prediction(predictions[-miss.list], no.miss)
} else {
	pred <- prediction(predictions, no.miss)
}
# Convert rate of positive predictions to percentage.
per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100
# Plot the lift chart.
plot(per, col="red", lty=1, xlab="Caseload (%)", add=TRUE)
legend("topright", c("structured.rf","dtm.rf","svd.rf"), col=c("green","blue","red"), lty=1:1, title="Models", inset=c(0.05, 0.05))

# calculate predicted values from the
# structured.rf and svd.rf modles                              
pred.structured<-predict(structured.rf,test)                                  
pred.svd<-predict(svd.rf,test.svd)
# classification matrix for training data
# for structured.rf
table(pred.structured,
      test$Suspicion_Flag,
      dnn=c("Predicted","Actual"))
# classification matrix for training data
# for structured.rf
table(pred.svd,
      test.svd$Suspicion_Flag,
      dnn=c("Predicted","Actual"))


set.seed(0)
KMeans <- kmeans(dtm.projection, centers=10,nstart=10)


#the wordcloud package offers a nice visualization function
library(wordcloud)
#print a wordcloud 
v <- colSums(dtm.to.Matrix(ICdtm))
names(v)<-dimnames(ICdtm)$Terms
color.list <- brewer.pal(8,"Dark2")
wordcloud(names(v),v,max.words=100,random.order=FALSE,colors=color.list)

#for fraud cases
dtm2<-dtm.to.Matrix(ICdtm)[insurance$Suspicion_Flag==1,]
v <- colSums(dtm2)
names(v)<-(dimnames(ICdtm)$Terms)
color.list <- brewer.pal(8,"Dark2")
wordcloud(names(v),v,max.words=100,random.order=FALSE,colors=color.list)

#no fraud
dtm3<-dtm.to.Matrix(ICdtm)[insurance$Suspicion_Flag==0,]
v <- colSums(dtm3)
names(v)<-(dimnames(ICdtm)$Terms)
color.list <- brewer.pal(8,"Dark2")
wordcloud(names(v),v,max.words=100,random.order=FALSE,colors=color.list)