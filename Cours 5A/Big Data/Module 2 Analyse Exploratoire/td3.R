library("fpc")

royau <- read.table("wow-royaumes.csv", sep = ",", header = T, row.names = 1, dec =".")

summary(royau)

print ( head ( royau ) )

# 1.1 Chargement des données

pairs (royau[, c (8:12) ])

plot(royau[,c(8:12)])

royau.x<-royau[c(8:12)]

# a) On retire les NA
royau.x<-na.omit(royau.x)

# b) On normalise
royau.cr<-scale(royau.x)

# c) Le centrage-réduction permet de comparer la répartition des données de
# variables quantitatives différentes, sur la même échelle.
str(royau.cr)

# 1.2 CAH


d.royau<-dist(royau.cr)
cah.ward <- hclust (d.royau, method ="ward.D2")
par(cex = 0.3, mar = c(5, 8, 4, 1))
plot(cah.ward, xlab="",ylab="",main="",axes = FALSE)
par(cex=1)
title (xlab="Nom des régions", ylab ="Répartition", main ="Classification ascendante hiérarchique")
axis (2)

# On assigne les groupes
groupes.cah <- cutree(cah.ward, k =4)

# b) On affiche les 4 groupes
rect.hclust(cah.ward,4)

# c) On récupère chacun des groupes
groupe1<-names(groupes.cah[groupes.cah==1])
groupe2<-names(groupes.cah[groupes.cah==2])
groupe3<-names(groupes.cah[groupes.cah==3])
groupe4<-names(groupes.cah[groupes.cah==4])


groupes.kmeans <- kmeans(royau.cr, centers=4, nstart=5)
print(groupes.kmeans)

print(table(groupes.cah, groupes.kmeans$cluster))


inertie.expl <- rep(0, times =10)

for(k in 2:10) {
  clus <- kmeans(royau.cr, centers=k, nstart =5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}

plot (1:10 , inertie.expl, type ="b", xlab ="Nb.de groupes", ylab ="%inertie expliquee ")

# a) On peut également estimer le meilleur groupe avec la silhouette
kr<-kmeansruns(royau.cr)
plot(kr$crit,type="b")

# b) On remarque que d'après le critère de la silhouette, il aurait pu être
# préférable de choisir 5 groupes car la silhouette est plus haute qu'à 4 ou 6

# 1.5)

stat.comp <- function (x, y) {
 K <- length (unique (y))
 n <- length (x)
 m <- mean (x)
 TSS <- sum ((x - m) ^2)
 nk <- table (y)
 mk <- tapply (x, y, mean)
 BSS <- sum ( nk * (mk - m) ^2)
 result <- c (mk, 100.0* BSS/ TSS )
 names (result) <- c(paste("G", 1:K),"% epl .")
 return (result)
}

stat.comp(royau.x,royau.x)