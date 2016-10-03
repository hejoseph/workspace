###TD 3 5052 : Analyse exploratoire
###Correction Emmanuelle. 

#charger les données - attention aux options
royau <- read.table('C:/Users/claey/Documents/cour/My TD/TD3/wow-royaumes.csv', sep =',',header=T,row.names=1,dec=".")

#afficher les premières lignes
print(head(royau))

#stat. descriptives
print(summary(royau))

#graphique - croisement deux à deux
pairs(royau[,c(8:12)])

royau.x <- royau[c(8:12)]

#1.1) a)
#Omission des variables avec NA
royau.x <- royau.x[complete.cases(royau.x),]
#1.1) b)
#centrage réduction des données
royau.cr <- scale(royau.x,center=T,scale=T)
#1.1) c)
#un centrage réduction sur les données bruts 
#permet de ne pas avoir une analyse faussée par des écarts importants pour certaines variables 

#distance entre individus
d.royau <- dist(royau.cr)

#CAH - critère de Ward
cah.ward <- hclust(d.royau,method="ward.D2")

# reduced label size
par(cex=0.3, mar=c(5, 8, 4, 1))
plot(cah.ward, xlab="", ylab="", main="", sub="", axes=FALSE)
par(cex=1)
title(xlab="xlab", ylab="ylab", main="main")
axis(2)

#découpage en 4 groupes
groupes.cah <- cutree(cah.ward,k=4)

########

#Alternative d'affichage parmis de nombreuses solutions
#1.2 a) 
library(ape)
# plot basic tree
par(cex=0.3, mar=c(5, 8, 4, 1))
plot(as.phylo(cah.ward), cex = 0.9, label.offset = 1)

#1.2 b) 
# reduced label size
par(cex=0.3, mar=c(5, 8, 4, 1))
plot(cah.ward, xlab="", ylab="", main="", sub="", axes=FALSE)
par(cex=1)
title(xlab="xlab", ylab="ylab", main="main")
axis(2)
#découpage en 4 groupes
groupes.cah <- cutree(cah.ward,k=4)


#dendrogramme avec matérialisation des groupes
rect.hclust(cah.ward,k=4)

#1.2 c)
#liste des groupes
print(sort(groupes.cah))

####
#k-means avec les données centrées et réduites
#center = 4 - nombre de groupes demandés
#nstart = 5 - nombre d'essais avec différents individus de départ

groupes.kmeans <- kmeans(royau.cr,centers=4,nstart=5)

#affichage des résultats
print(groupes.kmeans)

#correspondance avec les groupes de la CAH
print(table(groupes.cah,groupes.kmeans$cluster))

#Correspondance CAH - K -  Means Le groupe 4 de la CAH coïncide avec le groupe 1 des K-  Means
#Après, il y a certes des correspondances, 
#mais elles ne sont pas exactes.
#Remarque :
# Il se peut que vous n'ayez pas 
#exactement les mêmes résultats avec les K-  Means.



#1.3 a) & b)
#Je vous laisse consulter la fonction help :)
help(kmeans)
######################################
# K-MEANS - DETECTION NB. DE GROUPES
######################################

#(1)évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(royau.cr,centers=k,nstart=5)  
  inertie.expl[k] <- clus$betweenss/clus$totss
}

#graphique
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")

#1.4 a)
#Largeur moyenne de silhouette
#utilisation du package fpc 
library(fpc)
#évaluation des solutions
sol.kmeans <- kmeansruns(royau.cr,krange=2:10,criterion="ch")
#graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

#1.4 b)
#On cherche sur la courbe d'inertie expliquée, la valeur avant d'avoir une tangente quasi nulle
#On trouve la valeur 5.
#Sur le deusième graphique, on cherche le "pic" avant la petite décroissance de la croube
#On tombe également sur 5. 



#fonction de calcul des stats
stat.comp <- function(x,y){
  #nombre de groupes
  K <- length(unique(y))
  #nb. d'observations
  n <- length(x)
  #moyenne globale
  m <- mean(x)
  #variabilité totale
  TSS <- sum((x-m)^2)
  #effectifs conditionnels
  nk <- table(y)
  #moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #variabilité expliquée
  BSS <- sum(nk * (mk - m)^2)
  #moyennes + prop. variance expliquée
  result <- c(mk,100.0*BSS/TSS)
  #nommer les élements du vecteur
  names(result) <- c(paste("G",1:K),"% epl.")
  #renvoyer le résultat
  return(result)
}

#1.5 a)
#appliquer stat.comp aux variables de
#la base originelle royau.x
#et non pas aux variables centrées et réduites
print(sapply(royau.x,stat.comp,y=groupes.cah))
#1.5 b)
#Le groupe 1 est plutot un niveau pour joueurs "faibles"
#Le groupe 2 est ouvert à tout type de niveau
#Le groupe 3 est plutot un niveau pour joueurs "moyens"
#Le groupe 4 est plutot un niveau pour joueurs "forts"

#NB : si vous mettez k=5 (optimal) vous aurez de groupe "moyen" qui 
#deviendra "moyen faible" et "moyen fort"

###
#ACP
acp <- princomp(royau.x,cor=T,scores=T)

#screeplot - 2 axes retenus
plot(1:5,acp$sdev^2,type="b",xlab="Nb. de facteurs",ylab="Val. Propres")

#biplot
biplot(acp,cex=0.65)

#positionnement des groupes dans le plan factoriel avec etiquettes des points
plot(acp$scores[,1],acp$scores[,2],type="n",xlim=c(-7,7),ylim=c(-7,7))
text(acp$scores[,1],acp$scores[,2],col=c("red","green","blue","black")[groupes.cah],cex=0.65,labels=rownames(royau.x),xlim=c(-7,7),ylim=c(-7,7))

###
#1.6 a) Ici on observe que Isle of Quel'Danas  est isolé dans la repésentation 
#graphique.
#En vérifiant dans la bdd, cette donnée se comporte de maniere incohérente, 
#                  Min_req_level Min_rec_level Max_rec_level Min_bot_level Max_bot_level
# Isle of Quel'Danas        1      70               0             70             0
#Le niveau min est supperieur au niveau max.

#1.6 b)On retrouve bien une cohérence entre hclust et l'ACP, bien que le nombre de 
#k groupes ne soit pas "optimal" (5 aurait été meillieur)
#L'interet de l'ACP est de pouvoir observer rapidement les variables "étranges" par rapport aux autres variables dans
#différentes classes. 

