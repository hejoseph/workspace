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
#V�rifier si les donn�es suivent une distribution selon la loi normale
#Si une des variables ne suit pas la loi normal on peut faire une boxplot pour v�rifier les valeurs extremes et continuer l'ACP quand m�me
#1.1-b)
#Le test de Shapiro-Wilk est un test permettant de savoir si une s�rie de donn�es suit une loi normale.Pour
#Pour les petits effectifs (n<50),SPSS proc�de
#au calcul exact
#Pour les effectifs de taille mod�r�e, la fonction de R utlise un autre
#algorithme et pousse le teste jusqu'� n<5000.
#Pour Shapiro Wilk l'hypoth�se H0 de normalit� est v�rifi�e quand la valeur W est �lev�e (au contraire du test de Kolmogorov-Smirnov).
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
#S.Astringente et S.Amere corr�lation lin�aire positive
#S.Sucree et S.Amere corr�lation lin�aire n�gative

#1.2.b)
#CF cours : une acp doit r�aliser un centrage r�duction sur les donn�es bruts 
#pour ne pas avoir une analyse fauss�e par des �carts importants 
#dus � une ou plusieurs variable
#1.2.c)Un acp apr�s centrage r�duction premettera une meillieur interpr�tation des r�sultats
#que celle sans centrage r�duction. Notez que par les fonctions d'acp de ade4 et FactomineR vous font un CR par d�faut. 



library("rgl")

#Avant centrage-reduction
#Repr�sentation 3D : S.Acide,S.Sucree,S.Amere
plot3d(cidre$S.Acide,cidre$S.Sucree,cidre$S.Amere,type="s",xlim=c(-10,10),ylim=c(-10,10),zlim=c(-10,10))

#1.3. a)
plot3d(ellipse3d(cor(cbind(cidre$S.Acide,cidre$S.Sucree,cidre$S.Amere))),col="grey",alpha=0.05,add=TRUE)

#Repr�sentation 3D : S.Astringente,S.Sucree,S.Amere
plot3d(cidre$S.Astringente,cidre$S.Sucree,cidre$S.Amere,type="s",xlim=c(-10,10),ylim=c(-10,10),zlim=c(-10,10))

#1.3. a)
plot3d(ellipse3d(cor(cbind(cidre$S.Astringente,cidre$S.Sucree,cidre$S.Amere))),col="grey",alpha=0.05,add=TRUE)

#pour info
#var, cov and cor compute the variance of x and the covariance or correlation of x and y if these are vectors. If x and y are matrices then the covariances (or correlations) between the columns of x and the columns of y are computed.

#1.3. b)
#On voit qu'il y a un d�calage entre les valeurs des donn�es bruts et leurs covariances. 
#On souhaite avoir une meilleures repr�sentations des cidres et des variables.
#et retrouver nos points autour de l'�llipse de corr�lation

#Apres Centrage r�duction
cidre.cr <- scale(cidre[, c(2:5)])
lims <- c(min(cidre.cr),max(cidre.cr))

#L'op�ration  de  centrage  consiste  �  enlever  la  moyenne  �  chaque  variable.  
#La fonction scale() permet d'�ffectuer directement cette op�ration 
#Apr�s un centrage Le nuage est maintenant centr�e autour de l'origine. 
#Mais comme la variabilit�e peut �tre beaucoup plus forte pour une colonne que pour une autre
# il faut diviser les valeurs par l'�cart-type. Toujours avec la fonction
#scale
attach(cidreR)

#Repr�sentation 3D : S.Acide,S.Sucree,S.Amere
plot3d(S.Acide,S.Sucree,S.Amere,type="s",xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3))
plot3d(ellipse3d(cor(cbind(S.Acide,S.Sucree,S.Amere))),col="grey",alpha=0.05,add=TRUE)

#Repr�sentation 3D : S.Astringente,S.Sucree,S.Amere
plot3d(S.Astringente,S.Sucree,S.Amere,type="s",xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3))
plot3d(ellipse3d(cor(cbind(S.Astringente,S.Sucree,S.Amere))),col="grey",alpha=0.05,add=TRUE)

#1.4 a).
#Les donn�es sont centr�es r�duites et on peut voir visuellement qu'elles sont concentr�es autour de
#'�llipse de corr�lation. Ce qui n'etait pas le cas avec le graphique sans centrage r�duction

##############
#Utiliser la fonction dudi.pca() de la librairie ade4[2] pour ex�cuter une ACP
#centr�e r�duite :
library(ade4)
acp <- dudi.pca(cidre[, c(2:5)], center=TRUE, scale=TRUE, scannf = FALSE, nf = 3)
names(acp)
acp$tab
#1.5 a)
#Le  data  frame tab contient  les  donn�es  du  tableau  initial  apr�s  centrage  et
#r�duction.

#1.5 b)
#Cette petite dif�rence est due � l'utilisation d'une variance en 1/n
#dans dudi.pca() contre une variance en 1/(n-1) dans scale()

#1.5 c)
#Pour retrouver exactement le tableau utilis� dans dudi.pca() faire
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
# Je vous laisse regarder gr�ce � la fonction help :)
help("dudi.pca")

#1.6 j)
##La valeur nf donne le nombre de facteurs conserv�s dans l'analyse :
acp$nf
# 3
round(acp$eig,2)
round(cumsum(acp$eig*10),2)
screeplot(acp)

#Analyse des variables
inertie <-inertia.dudi(acp, col.inertia=TRUE)
#[coordonn�es des variables]
round(acp$co,2)
#[ctr en %]
inertie$col.abs/100
#[qlt en %]
inertie$col.re/100

s.corcircle(acp$co,xax=1,yax=2)

#1.7 a)
#Un attribut est bien repr�sent� si son cosinus pour chacun des axes est fort.
#1.7 b)
#On prend le vecteur � la norme la plus r�duite dans le cercle ce corr�lation lin�aire
#Le sucre est ici le moins bien repr�sent�  
#1.7 c)
#On cherche l'ange teta (sur 0- Pi) le plus petit entre un vecteur et celui associ� � l'Astringente
#On trouve le vecteur associ� � la variable Amere
#Le plus corr�l� n�gativement corrspond � l'angle le plus
#grand. C'est donc la variable Sucr�e
#
#1.7 d) Les attributs qui ont contribu�s � l'axe F1 sont Amere et Astringente. 
#La variable Sucr�e est n�gativement corr�l� � l'Astringence, elle contribut �galement (mais de fa�on moindre) � cet axe
#e) Cet Axe permet de caract�riser l'amertume d'un cidre
#f)L'Acidit� � majoritairement contribu� � l'axe 2.
#g) Une acidit� forte est un cidre vif, faible elle rend un cidre moelleux (c'est pour cela qu'on voit la variable Sucr�e dans la partie positive de l'axe 2 )


s.label(acp$li, xax = 1, yax = 2)
s.label(acp$li, xax = 1, yax = 2, label=as.character(cidre$Type), clabel=1.5)
gcol <- c("red1", "red4","orange")
s.class(dfxy = acp$li, fac = cidre$Type, col = gcol, xax = 1, yax = 2)
scatter(acp)

#1.8 a)
#Un cidre sucr�e est un cidre plutot doux
#Au contraire un cidre brut est un cidre plutot am�re et astringent
#Un cidre demi-sec est en g�n�ral un cidre  pr�domin� par une acidit� rafra�chissante. Cependant cela peut varier selon les producteurs. 
#Vous pouvez v�rifier, on retrouve les m�me d�finitions sur wikip�dia si vous n'�tes pas Breton
#;)