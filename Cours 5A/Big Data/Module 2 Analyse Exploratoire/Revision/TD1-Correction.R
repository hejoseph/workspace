###TD 1 5052 : Analyse exploratoire
###Correction Emmanuelle. 


library(nortest)


cidreR <- read.csv('cidre.csv', sep =';')
cidre <- as.data.frame(cidreR)
summary(cidre)
cor.test(cidre$S.Sucree, cidre$S.Acide)
cor.test(cidre$S.Sucree, cidre$S.Amere)
cor.test(cidre$S.Sucree, cidre$S.Astringente)
cor.test(cidre$S.Acide, cidre$S.Amere)
cor.test(cidre$S.Acide, cidre$S.Astringente)
cor.test(cidre$S.Amere, cidre$S.Astringente)


#1.1-a)
#Vérifier si les données suivent une distribution selon la loi normale
#Si une des variables ne suit pas la loi normal on peut faire une boxplot pour vérifier les valeurs extremes et continuer l'ACP quand même
#1.1-b)
#Le test de Shapiro-Wilk est un test permettant de savoir si une série de données suit une loi normale.Pour
#Pour les petits effectifs (n<50),SPSS procède
#au calcul exact
#Pour les effectifs de taille modérée, la fonction de R utlise un autre
#algorithme et pousse le teste jusqu'à n<5000.
#Pour Shapiro Wilk l'hypothèse H0 de normalité est vérifiée quand la valeur W est élevée (au contraire du test de Kolmogorov-Smirnov).
#Il arrive que la p-valeur soit beaucoup plus grand que 0,05 
#ce qui signifie que vous ne pouvez pas conclure que la distribution 
#est normale

shapiro.test(cidre$S.Sucree)
shapiro.test(cidre$S.Acide)
shapiro.test(cidre$S.Amere)
shapiro.test(cidre$S.Astringente)


#1.2) 
round(cor(cidre[, c(2:5)]),2)
#1.2.a)par exemple 
#S.Astringente et S.Amere corrélation linéaire positive
#S.Sucree et S.Amere corrélation linéaire négative

#1.2.b)
#CF cours : une acp doit réaliser un centrage réduction sur les données bruts 
#pour ne pas avoir une analyse faussée par des écarts importants 
#dus à une ou plusieurs variable
#1.2.c)Un acp après centrage réduction premettera une meillieur interprétation des résultats
#que celle sans centrage réduction. Notez que par les fonctions d'acp de ade4 et FactomineR vous font un CR par défaut. 



library("rgl")

#Avant centrage-reduction
#Représentation 3D : S.Acide,S.Sucree,S.Amere
plot3d(cidre$S.Acide,cidre$S.Sucree,cidre$S.Amere,type="s",xlim=c(-10,10),ylim=c(-10,10),zlim=c(-10,10))

#1.3. a)
plot3d(ellipse3d(cor(cbind(cidre$S.Acide,cidre$S.Sucree,cidre$S.Amere))),col="grey",alpha=0.05,add=TRUE)

#Représentation 3D : S.Astringente,S.Sucree,S.Amere
plot3d(cidre$S.Astringente,cidre$S.Sucree,cidre$S.Amere,type="s",xlim=c(-10,10),ylim=c(-10,10),zlim=c(-10,10))

#1.3. a)
plot3d(ellipse3d(cor(cbind(cidre$S.Astringente,cidre$S.Sucree,cidre$S.Amere))),col="grey",alpha=0.05,add=TRUE)

#pour info
#var, cov and cor compute the variance of x and the covariance or correlation of x and y if these are vectors. If x and y are matrices then the covariances (or correlations) between the columns of x and the columns of y are computed.

#1.3. b)
#On voit qu'il y a un décalage entre les valeurs des données bruts et leurs covariances. 
#On souhaite avoir une meilleures représentations des cidres et des variables.
#et retrouver nos points autour de l'éllipse de corrélation

#Apres Centrage réduction
cidre.cr <- scale(cidre[, c(2:5)])
lims <- c(min(cidre.cr),max(cidre.cr))

#L'opération  de  centrage  consiste  à  enlever  la  moyenne  à  chaque  variable.  
#La fonction scale() permet d'éffectuer directement cette opération 
#Après un centrage Le nuage est maintenant centrée autour de l'origine. 
#Mais comme la variabilitée peut être beaucoup plus forte pour une colonne que pour une autre
# il faut diviser les valeurs par l'écart-type. Toujours avec la fonction
#scale
attach(cidreR)

#Représentation 3D : S.Acide,S.Sucree,S.Amere
plot3d(S.Acide,S.Sucree,S.Amere,type="s",xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3))
plot3d(ellipse3d(cor(cbind(S.Acide,S.Sucree,S.Amere))),col="grey",alpha=0.05,add=TRUE)

#Représentation 3D : S.Astringente,S.Sucree,S.Amere
plot3d(S.Astringente,S.Sucree,S.Amere,type="s",xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3))
plot3d(ellipse3d(cor(cbind(S.Astringente,S.Sucree,S.Amere))),col="grey",alpha=0.05,add=TRUE)

#1.4 a).
#Les données sont centrées réduites et on peut voir visuellement qu'elles sont concentrées autour de
#'éllipse de corrélation. Ce qui n'etait pas le cas avec le graphique sans centrage réduction

##############
#Utiliser la fonction dudi.pca() de la librairie ade4[2] pour exécuter une ACP
#centrée réduite :
library(ade4)
acp <- dudi.pca(cidre[, c(2:5)], center=TRUE, scale=TRUE, scannf = FALSE, nf = 3)
names(acp)
acp$tab
#1.5 a)
#Le  data  frame tab contient  les  données  du  tableau  initial  aprés  centrage  et
#réduction.

#1.5 b)
#Cette petite diférence est due à l'utilisation d'une variance en 1/n
#dans dudi.pca() contre une variance en 1/(n-1) dans scale()

#1.5 c)
#Pour retrouver exactement le tableau utilisé dans dudi.pca() faire
var.n <- function(x) sum((x-mean(x))^2)/length(x)
scale.n <- function(x) (x - mean(x))/sqrt(var.n(x))
head(apply(cidre[, c(2:5)], 2, scale.n))
acp$eig
sum(acp$eig)
(pve <- 100*acp$eig/sum(acp$eig))
cumsum(pve)



#1.6 a)
#97.68131
#1.6 b) - i)
# Je vous laisse regarder grâce à la fonction help :)
help("dudi.pca")

#1.6 j)
##La valeur nf donne le nombre de facteurs conservés dans l'analyse :
acp$nf
# 3
round(acp$eig,2)
round(cumsum(acp$eig*10),2)
screeplot(acp)

#Analyse des variables
inertie <-inertia.dudi(acp, col.inertia=TRUE)
#[coordonnées des variables]
round(acp$co,2)
#[ctr en %]
inertie$col.abs/100
#[qlt en %]
inertie$col.re/100

s.corcircle(acp$co,xax=1,yax=2)

#1.7 a)
#Un attribut est bien représenté si son cosinus pour chacun des axes est fort.
#1.7 b)
#On prend le vecteur à la norme la plus réduite dans le cercle ce corrélation linéaire
#Le sucre est ici le moins bien représenté  
#1.7 c)
#On cherche l'ange teta (sur 0- Pi) le plus petit entre un vecteur et celui associé à l'Astringente
#On trouve le vecteur associé à la variable Amere
#Le plus corrélé négativement corrspond à l'angle le plus
#grand. C'est donc la variable Sucrée
#
#1.7 d) Les attributs qui ont contribués à l'axe F1 sont Amere et Astringente. 
#La variable Sucrée est négativement corrélé à l'Astringence, elle contribut également (mais de façon moindre) à cet axe
#e) Cet Axe permet de caractériser l'amertume d'un cidre
#f)L'Acidité à majoritairement contribué à l'axe 2.
#g) Une acidité forte est un cidre vif, faible elle rend un cidre moelleux (c'est pour cela qu'on voit la variable Sucrée dans la partie positive de l'axe 2 )


s.label(acp$li, xax = 1, yax = 2)
s.label(acp$li, xax = 1, yax = 2, label=as.character(cidre$Type), clabel=1.5)
gcol <- c("red1", "red4","orange")
s.class(dfxy = acp$li, fac = cidre$Type, col = gcol, xax = 1, yax = 2)
scatter(acp)

#1.8 a)
#Un cidre sucrée est un cidre plutot doux
#Au contraire un cidre brut est un cidre plutot amère et astringent
#Un cidre demi-sec est en général un cidre  prédominé par une acidité rafraîchissante. Cependant cela peut varier selon les producteurs. 
#Vous pouvez vérifier, on retrouve les même définitions sur wikipédia si vous n'êtes pas Breton
#;)