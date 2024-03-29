###TD 2 5052 : Analyse exploratoire
###Correction Emmanuelle.

dataP<- read.csv('afc_tache_menageres.csv', sep =';')

barplot(t(dataP[,-1]),beside=T,names=dataP$Task, col = c("lightblue", "mistyrose",
                                                         "lightcyan", "lavender"),
        legend = colnames(dataP[,2:5]))

#1.1 a)
#Il semblerait que plusieurs t�ches soit majoritairement affect�es aux femmes (ex Laundry, dinner)
#A contrario certaines t�ches sont plus souvents affect�es aux hommes.
#Il y a aussi des t�ches qui sont plus "�quitables"
#On suppose donc des corr�lations entre plusieurs t�ches m�nag�res

#1.1 b)
resu.chi2 <- chisq.test(dataP[,-1]) 
#1.1 c)
#La p-value extr�ement faible sgnifie que l'hypoth�se d'ind�pendance
#entre certaines t�ches et personne assign�e � la t�che peut etre rejet�e "sans trop de risque" 
#(ce qui est attendu) 
#Notons tout de m�me la port�ee limit�e de ce test car les 
#donn�es  �tudi�es sont difficilement assimilables 
#`a un  �chantillon al�atoire.

#1.1 d) 
#- deux variables qualitatives
#- sous la forme d'une table de contingence
#- pr�sence de corr�lation entre des variables
###
library(FactoMineR)
res = CA(dataP[,2:5])
plot.CA(res,xax = 1, yax = 2, clabel=1.5)

barplot (res$eig[,2], names=paste("Dim",1:length(res
                                                 $eig[,2])), main="Inertie
         (en %) des axes factoriels", col="orange", border="white"
)

#1.2 a) 
row.names(dataP) <- dataP$Task
res = CA(dataP[,2:5])
barplot (res$eig[,2], names=paste("Dim",1:length(res
                                                 $eig[,2])), main="Inertie
         (en %) des axes factoriels", col="orange", border="white" )
#1.2.b)
#La Figure met en  �vidence deux axes pr�dominants, cumulant 
#environ 85% de l'inertie

#1.2. c)
res$eig$`cumulative percentage of variance`

#1.2.d)
#v  de Cramer est �gale
sqrt(resu.chi2$statistic/(sum(dataP[1:13,2:5])*3))


#1.2.e)
#le V de Cramer, contrairement au Chi�, il reste stable si l'on augmente la taille 
#de l'�chantillon dans les m�mes proportions inter-modalit�s. 
#Plus V est proche de z�ro, plus il y a ind�pendance entre les deux 
#variables �tudi�es. Il vaut 1 en cas de compl�te d�pendance puisque le 
#chi� est alors �gal au Chi� max (dans un tableau 2 � 2), 
#il prend une valeur comprise entre -1 et 1).
#1.2.f)
#on peut �galement calculer directement le V de Cramer `a l'aide du package
#questionr:

library(questionr)
cramer.v(dataP[1:13,2:5])

#1.3 a)
res2 = CA(dataP[,2:5], axes = c(2,3))
#1.3 b)
res$col$coord  # Affiche les coordonn�es des modalit�s colonnes
#1.3 c)
res$col$cos2 #qualit� de projection des modalit�s colonnes
#1.3 d)
res$col$contrib # contribution des modalit�s colonnes aux axes factoriels
#1.3 e)
res$row$coord # coordonn�es des modalit�s lignes dans le plan factoriel
#1.3 f)
res$row$cos2 # Qualit� de repr�sentation des modalit�s lignes
#1.3 g)
res$row$contrib # Contribution des modalit�s lignes aux axes factoriels

#Commentaire
res = CA(dataP[,2:5])
#L'axe 1 s�pare plut�t le sexe 
#L'axe 2 s�pare plut�t le nombre de personne qui r�alise la t�che ({Homme, Femme, Alternative} VS Jointly)
#Par exemple les vacances sont g�n�ralement organis�es par deux personnes ensembles
#Sans entrer dans un d�bat sur le sexisme, on voit que le m�nage est plut�t fait par des femmes, 
#et les t�ches bricolage et conduite sont des t�ches "masculine". 

################# EXO 2
poke <- read.csv('Pokemon.csv',  na.strings=c("","NA"), sep =',')
summary(poke)

library(ade4)
library(adegraphics)
poke <- as.data.frame(poke)
poke$Generation <- as.factor(poke$Generation)
poke.x <- poke[,c(3,12,13)]
summary(poke)

acmtot <- dudi.acm(poke.x,scannf=FALSE)
barplot(acmtot$eig)
score(acmtot,xax=1)
head(inertia.dudi(acmtot)$TOT)

par(mfrow = c(1,2), mar=c(5,6,2,0), cex=0.70)
barplot(acmtot$cr[,1], horiz=TRUE, names.arg = colnames(poke.x), las = 1,
        main = "Premier facteur", col = "lightblue",
        xlab = "Rapport de corr�lation")


#2.2 a)
barplot(acmtot$cr[,2], horiz=TRUE, xlim = c(0,1), names.arg = colnames(poke.x), las = 1,
        main = "Deuxi�me facteur", col = "lightblue",
        xlab = "Rapport de correlation")
#2.2 b)
#Premier facteur : Type 1 (+60%) et legendary (50%)
#Deusieme facteur : Type 1 (+60%) et g�n�ration (60%)
#On voit que le deusieme axe est plus tranch� que le premier


#2.2 c)
#passage en tableau de Burt
table(poke.x)

library(vcd)
assocstats(table(poke.x))
# On peut observer le V de cramer et le coefficient de contingence.
#Ce valeurs d�montre qu'il existe de forte cor�lation dans les donn�es

#2.2 d) Corr�lation sur les variables quantitatives
round(cor(subset(poke,select=c(7:11))),2)

####
library(PCAmixdata)
pcamix.temp<- PCAmix(subset(poke,select=c(7:11)) , subset(poke,select=c(3)))
#valeurs propres
print(round(pcamix.temp$eig))
#corr�lations
print(round(pcamix.temp$quanti.cor))
#coord. des modalit�s dudi.mix de ADE4
print(round(pcamix.temp$categ.coord))

#2.3 a)
row.names(poke) <- poke$Name
pcamix.temp<- PCAmix(subset(poke,select=c(7:11)) , subset(poke,select=c(2)))
#valeurs propres
print(round(pcamix.temp$eig))
#corr�lations
print(round(pcamix.temp$quanti.cor))
#Dim 1 : Corr�lation positive avec toutes les variables 
#Dim 2 : Corr�lation negative avec Defense et  positive pour Speed


#coord. des modalit�s dudi.mix de ADE4
print(round(pcamix.temp$categ.coord))
#2.3 b)
#Pikatchu a les coordon�es suivantes
#Pikachu                     -1    1    0    1    0
#On peut donc dire que 
#Ce n'est pas un pokemon "Fort" compar� aux autres (qu'il est moyen partout)
#Il est plut�t rapide
#2.3 c)
#####
poke$X. <- NULL
res <- FAMD(poke)
summary(res)

#2.3 d)
#On peut comparer les pokemons par rapport aux autres et cette fois en int�grants toutes les
#variables, quanti et quali (attention on enl�ve leurs ID X qui n'est pas une information � utiliser)
#Pour la fonction FAMD je vous laisse regarder dans la doc
help(FAMD)
