DataURL<-read.csv("urls.csv",sep=",")
df<-as.data.frame(DataURL)
dfUrl<-df[!(is.na(df$Text)|df$Text==""),]
dfUrl<-dfUrl[!dfUrl$Text=="0",]


library(tm)
library(ggplot2)
library(lsa)
library(scatterplot3d)
library(SnowballC)
docs<-Corpus(VectorSource(dfUrl$Text[1:nrow(dfUrl)]))
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,tolower)
docs<-tm_map(docs,removeWords,stopwords("french"))
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,PlainTextDocument)
dtm<-DocumentTermMatrix(docs)
tdm<-TermDocumentMatrix(docs)


library(topicmodels)
#SetparametersforGibbssampling
burnin<-2000
iter<-500
thin<-500
seed<-list(2003,5,63,100001,765)
nstart<-5
best<-TRUE
#Numberoftopics
k<-5
#RunLDAusingGibbssampling
ldaOut<-LDA(dtm,k,method="Gibbs",control=list(nstart=nstart,seed=seed,best=best,burnin=burnin,iter=iter,thin=thin))
#writeoutresults
#docstotopics
ldaOut.topics<-as.matrix(topics(ldaOut))
ldaOut.topics

#top10termsineachtopic
ldaOut.terms<-as.matrix(terms(ldaOut,10))
ldaOut.terms

topicProbabilities

findAssocs(dtm,"franois",corlimit=0.7)


#termesencolonne
matrice_docs_termes<-(dtm)
#top50desmotsfrquents
findFreqTerms(matrice_docs_termes,50)


inspect(removeSparseTerms(matrice_docs_termes,0.4))


Zipf_plot(matrice_docs_termes)


Heaps_plot(matrice_docs_termes)


#######################Cluster###################"
mtd4.TfIdf<-(DocumentTermMatrix(docs,control=list(weighting=
weightTfIdf)))
dim(mtd4.TfIdf)
[1]26518629

dist4<-dist(t(topicProbabilities),method="euclidean")
dist4


hc4<-hclust(dist4,method="ward.D2")
plot(hc4)


library(wordcloud)
#Wordclouds
wordcloud(docs,
+scale=c(5,0.1),rot.per=0.35,
+min.freq=5,max.words=50,use.r.layout=FALSE,
+colors=brewer.pal(8,"Spectral")
+)
