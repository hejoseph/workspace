royau  <- read.table("wow-royaumes.csv", sep =",",header=T,row.names=1,dec =".")
royau.x <- royau[c(8:12)]

royau.x<-na.omit(royau.x)
royau.cr<-scale(royau.x)
d.royau <- dist(royau.cr)
cah.ward <- hclust(d.royau ,method ="ward.D2")



# par(cex=0.3, mar=c(5, 8, 4, 1))
# plot(cah.ward , xlab="", ylab="", main="", sub="", axes=FALSE)
# par(cex =1)
# title(xlab="xlab", ylab="ylab", main="main")
axis (2)
groupes.cah <- cutree(cah.ward ,k=4)

rect.hclust(cah.ward,4)
groupes.kmeans <- kmeans(royau.cr ,centers=4,nstart =5)

print(table(groupes.cah ,groupes.kmeans$cluster))

inertie.expl  <- rep(0,times =10)
for (k in  2:10){
clus  <- kmeans(royau.cr,centers=k,nstart =5)
inertie.expl[k] <- clus$betweenss/clus$totss
}
plot (1:10 , inertie.expl ,type="b",xlab="Nb. de  groupes",ylab ="%inertie  expliquee ")


stat.comp  <- function(x,y){
    K <- length(unique(y))
    n <- length(x)
    m <- mean(x)
    TSS  <- sum((x-m)^2)
    nk <- table(y)
    mk <- tapply(x,y,mean)
    BSS  <- sum(nk * (mk - m)^2)
    result  <- c(mk ,100.0* BSS/TSS)
    names(result) <- c(paste ("G",1:K) ,"% epl .")
    return(result)
}



acp<-princomp(royau.x,cor=T,scores=T)

plot (1:5, acp$sdev^2,type="b",xlab="Nb. de  facteurs",ylab="Val.Propres ")

biplot(acp ,cex =0.65)

plot(acp$scores [,1], acp$scores [,2],type="n",xlim=c(-5,5),ylim=c(-5,5))

text(acp$scores [,1], acp$scores [,2],col=c("red","green","blue","black")[groupes.cah],cex=0.65, labels=rownames(royau.x),xlim=c(-5,5),ylim=c(-5,5))