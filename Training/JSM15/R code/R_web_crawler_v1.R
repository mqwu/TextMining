#make sure offline version of Java is downloaded from
#https://java.com/en/download/manual.jsp
#install new packages
textpack4<-c("rJava","boilerpipeR","XML","RCurl")
install.packages(textpack4,dependencies=TRUE)

#Enter a website (include http://)
search.target <- "http://www.adsurgo.com"

#How deep should the search go? (Run-time will increase exponentially here)
#0: crawl only the specified page. 1: crawl specified page and all other
#linked pages that are subsets of the submitted page. 2: same, in addition
#to all sites linked on the linked pages. etc.
crawl.depth <- 1

#Minimum number of characters per page: (filters out empty content)
min.char <- 100

#Select an extractor
#Changing the extractor will change the amount of text that is
#extracted/ignored from each page
#1-Default 2-Alternative Default 3-News Article
#4-Keep all text 5-Largest Content
extractor <- 1

#time (in seconds) to wait between retrieving pages
#in order to avoid being blocked by host site
sleep.time <- 0.2

if (Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
library(rJava)
library(boilerpipeR)
library(XML)
library(RCurl)
curl <- getCurlHandle()

getLinks <- function(URL) {
    doc <- htmlParse(URL)
    out <- unlist(doc['//@href'])
    title<-xpathSApply(doc, '//head/title', xmlValue, simplify = TRUE, encoding="UTF-8")
    free(doc)
    names(out) <- NULL
    list(out,title)
}
strcount <- function(x, pattern, split){

unlist(lapply(
    strsplit(x, split),
       function(z) na.omit(length(grep(pattern, z)))
   ))

}

  url<-search.target
 content <- getURL(url,curl=curl)
 content<-enc2utf8(content)
 LT<-getLinks(content)
 links<-sort(unique(LT[[1]]))
  if(extractor==1){
 extract <-CanolaExtractor(content)
 }else if(extractor==2){
 	 extract <- DefaultExtractor(content)
  }else if(extractor==3){
 	 extract <- ArticleExtractor(content)
 }else if(extractor==4){
 	 extract <- KeepEverythingExtractor(content)
 }else{
 	extract<-LargestContentExtractor(content)
 }
 extract<-gsub("[/,.:;\n\\]"," ",extract)
 title<-LT[[2]]
 df<-as.data.frame(cbind(url,extension=c(" "),title,text=extract,depth=0))
 df[,1]<-as.character(df[,1])
 df[,2]<-as.character(df[,2])
  df[,3]<-as.character(df[,3])
next_level_links<-c(NULL)

if(crawl.depth>0){
for(depth.level in 1:crawl.depth){
if(depth.level>1){
 links<-next_level_links
 }


filtered_links.1<-links[substr(links,1,1)=="/"]
filtered_links.1<-paste(url,filtered_links.1[substr(filtered_links.1,2,2)!="/"],sep="")
filtered_links.2<-links[substr(links,1,3)=="../"]
filtered_links.2<-substring(filtered_links.2,3)
filtered_links.2<-paste(url,filtered_links.2[substr(filtered_links.2,2,2)!="/"],sep="")
filtered_links.3<-links[substr(links,1,nchar(url))==url]
filtered_links.4<-links[substr(links,3,nchar(url)-5)==substring(url,8)]
filtered_links.4<-paste("http://",substring(filtered_links.4,3),sep="")
if(grepl(".com",url)){
	base_url<-paste(strsplit(url,".com",fixed=TRUE)[[1]][1],".com",sep="")
}
if(grepl(".org",url)){
	base_url<-paste(strsplit(url,".org",fixed=TRUE)[[1]][1],".org",sep="")
}
if(grepl(".gov",url)){
	base_url<-paste(strsplit(url,".gov",fixed=TRUE)[[1]][1],".gov",sep="")
}
filtered_links.5<-paste(base_url,links,sep="")
filtered_links <- sort(unique(tolower(c(filtered_links.1,filtered_links.2,filtered_links.3,filtered_links.4,filtered_links.5))))
if(depth.level==1){
master_links<-filtered_links
}else{
filtered_links<-filtered_links[!(filtered_links%in%master_links)]
master_links<-unique(c(master_links,filtered_links))
next_level_links<-c(NULL)
}
counter<-0
for(i in filtered_links){
counter<-counter+1
linked.url<-i
if(strcount(linked.url,"htm","/")>1) next
if(length(strsplit(linked.url,"http")[[1]])>2) next
if(length(strsplit(linked.url,"//")[[1]])>3) next
Sys.sleep(sleep.time)
print(linked.url)
flush.console()
if(class(try(content <- getURL(linked.url,curl=curl),silent=TRUE))=="try-error") next
content<-enc2utf8(content)
 if(class(try(LT<-getLinks(content),silent=TRUE))=="try-error") next
 title<-LT[[2]]
 if(class(title)=="list") title<-c(" ")
 links.2<-sort(unique(LT[[1]]))
 next_level_links<-c(next_level_links,links.2)
  if(extractor==1){
 extract <-CanolaExtractor(content)
 }else if(extractor==2){
 	 extract <- DefaultExtractor(content)
  }else if(extractor==3){
 	 extract <- ArticleExtractor(content)
 }else if(extractor==4){
 	 extract <- KeepEverythingExtractor(content)
 }else{
 	 extract <- LargestContentExtractor(content)
 }
 extract<-gsub("[/,.:;\n\\]"," ",extract)
 if(nchar(extract)>=min.char) df<-rbind(df,cbind(url=linked.url,extension=i,title=title,text=extract,depth=depth.level))
 }
df<-df[!duplicated(df$text),]
gc()



next_level_links<-sort(unique(tolower(next_level_links)))
next_level_links<-next_level_links[!(next_level_links%in%master_links)]

}
}#loop over depth

write.csv(df,file=paste("web_crawler_output_",gsub(":",".",date()),".csv",sep=""))

