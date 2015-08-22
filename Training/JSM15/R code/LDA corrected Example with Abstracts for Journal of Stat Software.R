#Get abstracts from Journal of Stat Software
library("OAIHarvester")
x <- oaih_list_records("http://www.jstatsoft.org/oai")
JSS_papers <- oaih_transform(x[, "metadata"])
JSS_papers <- JSS_papers[order(as.Date(unlist(JSS_papers[, "date"]))), ]
JSS_papers <- JSS_papers[grep("Abstract:", JSS_papers[, "description"]), ]
JSS_papers[, "description"] <- sub(".*\nAbstract:\n", "",
unlist(JSS_papers[, "description"]))

#Use only those through Aug 2010
JSS_papers <- JSS_papers[JSS_papers[,"date"] < "2010-08-05",]
JSS_papers <- JSS_papers[sapply(JSS_papers[, "description"],
Encoding) == "unknown",]

#take out greek letters and subscripts with XML
library("tm")
library("XML")
remove_HTML_markup <-
function(s) tryCatch({
doc <- htmlTreeParse(paste("<!DOCTYPE html>", s),
asText = TRUE, trim = FALSE)
xmlValue(xmlRoot(doc))
}, error = function(s) s)
corpus <- Corpus(VectorSource(sapply(JSS_papers[, "description"],
remove_HTML_markup)))

# Use C locale for reproducibility
Sys.setlocale("LC_COLLATE", "C")

#Form Document Term Matrix with dimensions 348 x 4289
JSS_dtm <- DocumentTermMatrix(corpus,
control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
removeNumbers = TRUE, removePunctuation = TRUE))
dim(JSS_dtm)

# Look at column sums (word frequencies) and form weighted tf-idf
library("slam")
summary(col_sums(JSS_dtm))
term_tfidf <-
tapply(JSS_dtm$v/row_sums(JSS_dtm)[JSS_dtm$i], JSS_dtm$j, mean) *
log2(nDocs(JSS_dtm)/col_sums(JSS_dtm > 0))
summary(term_tfidf)

#  Min.   1st Qu.  Median   Mean   3rd Qu. Max.
# 0.01520 0.07472 0.09817 0.12210 0.13840 1.16500

JSS_dtm <- JSS_dtm[,term_tfidf >= 0.1]
JSS_dtm <- JSS_dtm[row_sums(JSS_dtm) > 0,]
summary(col_sums(JSS_dtm))

# should be close to 
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# 1.000 1.000 1.000 2.763 3.000 47.000

dim(JSS_dtm)
# Now try four different models for topics
library("topicmodels")
# Number of topics is 30 (usually less than 100)
k <- 30
SEED <- 2010
 jss_TM <-
list(VEM = LDA(JSS_dtm, k = k, control = list(seed = SEED)),
VEM_fixed = LDA(JSS_dtm, k = k,
control = list(estimate.alpha = FALSE, seed = SEED)),
Gibbs = LDA(JSS_dtm, k = k, method = "Gibbs",
control = list(seed = SEED, burnin = 1000,
thin = 100, iter = 1000)),
CTM = CTM(JSS_dtm, k = k,
control = list(seed = SEED,
var = list(tol = 10^-4), em = list(tol = 10^-3))))

# Look at alpha value of the fixed versus estimated VEM model--estimated is much smaller indicating few overall topics
sapply(jss_TM[1:2], slot, "alpha")

#  VEM       VEM_fixed
# 0.0123423 1.6666667

# Look at entropy--higher values indicate topics more evenly spread over documents
sapply(jss_TM, function(x)
mean(apply(posterior(x)$topics,
1, function(z) - sum(z * log(z)))))

#
# VEM       VEM_fixed  Gibbs     CTM
#0.3075525 3.1242627 3.2880912 0.2149500

# Finally,what we want--the words that form topics
Topic <- topics(jss_TM[["VEM"]], 1)
Terms <- terms(jss_TM[["VEM"]], 5)
Terms[,1:5]

#Volume 24 was a special issue statistical modeling of social networks--what topics are in that volume?
(topics_v24 <-
topics(jss_TM[["VEM"]])[grep("/v24/", JSS_papers[, "identifier"])])
most_frequent_v24 <- which.max(tabulate(topics_v24))

# Output shows these journal abastracts in V24 most prominently go to topic 21 though results may differ (e.g. 26)
# 243 244 245 246 247 248 249 250 251
# 21  25  21  21  27  21  21  23  21

# Top 10 terms from Topic 21 are 
terms(jss_TM[["VEM"]], 10)[, most_frequent_v24]

#[1] "network" "ergm" "graph"
#[4] "format" "econometr" "brief"
#[7] "hydra" "statnet" "imag"
#[10] "exponentialfamili"

NSF<-read.csv(file.choose())
# list the names of the variables in the data.frame
NSF
dtm.to.Matrix<- function(dtm){
    m <- sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v, dims = c(dtm$nrow, dtm$ncol))
    rownames(m)<-dtm$dimnames$Docs
    colnames(m)<-dtm$dimnames$Terms
    return(m)
}
library(irlba)
#to install Rstem, open an R session and select Packages > Install packages
library(SnowballC)
library(Matrix)
NSF_corpus<-Corpus(VectorSource(NSF$ï..text))
NSF_corpus
NSF_dtm<- DocumentTermMatrix(NSF_corpus,control=list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
removeNumbers = TRUE, removePunctuation = TRUE, weighting= weightTf))
#NSF_dtm<-removeSparseTerms(NSF_dtm, 0.99)
dim(NSF_dtm)
library("slam")
summary(col_sums(NSF_dtm))
term_tfidf <-
tapply(NSF_dtm$v/row_sums(NSF_dtm)[NSF_dtm$i], NSF_dtm$j, mean) *
log2(nDocs(NSF_dtm)/col_sums(NSF_dtm > 0))
summary(term_tfidf)
NSF_dtm <- NSF_dtm[,term_tfidf >= 0.1]
NSF_dtm <- NSF_dtm[row_sums(NSF_dtm) > 0,]
summary(col_sums(NSF_dtm))
# k ia number of topics--30 is default, can go to 100
k <- 30
SEED <- 2010
NSF_tm<-
list(VEM = LDA(NSF_dtm, k = k, control = list(seed = SEED)),
VEM_fixed = LDA(NSF_dtm, k = k,
control = list(estimate.alpha = FALSE, seed = SEED)),
Gibbs = LDA(NSF_dtm, k = k, method = "Gibbs",
control = list(seed = SEED, burnin = 1000,
thin = 100, iter = 1000)),
CTM = CTM(NSF_dtm, k = k,
control = list(seed = SEED,
var = list(tol = 10^-4), em = list(tol = 10^-3))))

# Look at alpha value of the fixed versus estimated VEM model--estimated is much smaller indicating few overall topics
sapply(NSF_tm[1:2], slot, "alpha")

# Look at entropy--higher values indicate topics more evenly spread over documents
sapply(NSF_tm, function(x)
mean(apply(posterior(x)$topics,
1, function(z) - sum(z * log(z)))))

# Variational Expectation Maximation
# Topic determines the most prominent topic weighting on each abstract
# Terms provides the top terms associated with a specifi topic
TopicVEM <- topics(NSF_tm[["VEM"]], 1)
TermsVEM <- terms(NSF_tm[["VEM"]], 10)
TermsVEM[1:10,1:5]
TopicVEM[1:15]
# Gibbs Sampling algorithm with Markov Chain Monte Carlo
TopicGibbs <- topics(NSF_tm[["Gibbs"]], 1)
TermsGibbs <- terms(NSF_tm[["Gibbs"]], 10)
TermsGibbs[1:10,1:5]
TopicGibbs[1:15]
# Correlated Topic Model
TopicCTM <- topics(NSF_tm[["CTM"]], 1)
TermsCTM <- terms(NSF_tm[["CTM"]], 10)
TermsCTM[1:10,1:5]
TopicCTM[1:15]






