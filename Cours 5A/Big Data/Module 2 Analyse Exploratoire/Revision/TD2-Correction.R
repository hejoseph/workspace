###TD 2 5052 : Analyse exploratoire
###Correction Emmanuelle.

dataP<- read.csv('afc_tache_menageres.csv', sep =';')

barplot(t(dataP[,-1]),beside=T,names=dataP$Task, col = c("lightblue", "mistyrose",
                                                         "lightcyan", "lavender"),
        legend = colnames(dataP[,2:5]))

#1.1 a)
#Il semblerait que plusieurs tâches soit majoritairement affectées aux femmes (ex Laundry, dinner)
#A contrario certaines tâches sont plus souvents affectées aux hommes.
#Il y a aussi des tâches qui sont plus "équitables"
#On suppose donc des corrélations entre plusieurs tâches ménagères

#1.1 b)
resu.chi2 <- chisq.test(dataP[,-1]) 
#1.1 c)
#La p-value extréement faible sgnifie que l'hypothèse d'indépendance
#entre certaines tâches et personne assignée à la tâche peut etre rejetée "sans trop de risque" 
#(ce qui est attendu) 
#Notons tout de même la portéee limitée de ce test car les 
#données  étudiées sont difficilement assimilables 
#`a un  échantillon aléatoire.

#1.1 d) 
#- deux variables qualitatives
#- sous la forme d'une table de contingence
#- présence de corrélation entre des variables
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
#La Figure met en  évidence deux axes prédominants, cumulant 
#environ 85% de l'inertie

#1.2. c)
res$eig$`cumulative percentage of variance`

#1.2.d)
#v  de Cramer est égale
sqrt(resu.chi2$statistic/(sum(dataP[1:13,2:5])*3))


#1.2.e)
#le V de Cramer, contrairement au Chi², il reste stable si l'on augmente la taille 
#de l'échantillon dans les mêmes proportions inter-modalités. 
#Plus V est proche de zéro, plus il y a indépendance entre les deux 
#variables étudiées. Il vaut 1 en cas de complète dépendance puisque le 
#chi² est alors égal au Chi² max (dans un tableau 2 × 2), 
#il prend une valeur comprise entre -1 et 1).
#1.2.f)
#on peut également calculer directement le V de Cramer `a l'aide du package
#questionr:

library(questionr)
cramer.v(dataP[1:13,2:5])

#1.3 a)
res2 = CA(dataP[,2:5], axes = c(2,3))
#1.3 b)
res$col$coord  # Affiche les coordonnées des modalités colonnes
#1.3 c)
res$col$cos2 #qualité de projection des modalités colonnes
#1.3 d)
res$col$contrib # contribution des modalités colonnes aux axes factoriels
#1.3 e)
res$row$coord # coordonnées des modalités lignes dans le plan factoriel
#1.3 f)
res$row$cos2 # Qualité de représentation des modalités lignes
#1.3 g)
res$row$contrib # Contribution des modalités lignes aux axes factoriels

#Commentaire
res = CA(dataP[,2:5])
#L'axe 1 sépare plutôt le sexe 
#L'axe 2 sépare plutôt le nombre de personne qui réalise la tâche ({Homme, Femme, Alternative} VS Jointly)
#Par exemple les vacances sont généralement organisées par deux personnes ensembles
#Sans entrer dans un débat sur le sexisme, on voit que le ménage est plutôt fait par des femmes, 
#et les tâches bricolage et conduite sont des tâches "masculine". 

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
        xlab = "Rapport de corrélation")


#2.2 a)
barplot(acmtot$cr[,2], horiz=TRUE, xlim = c(0,1), names.arg = colnames(poke.x), las = 1,
        main = "Deuxième facteur", col = "lightblue",
        xlab = "Rapport de correlation")
#2.2 b)
#Premier facteur : Type 1 (+60%) et legendary (50%)
#Deusieme facteur : Type 1 (+60%) et génération (60%)
#On voit que le deusieme axe est plus tranché que le premier


#2.2 c)
#passage en tableau de Burt
table(poke.x)

library(vcd)
assocstats(table(poke.x))
# On peut observer le V de cramer et le coefficient de contingence.
#Ce valeurs démontre qu'il existe de forte corélation dans les données

#2.2 d) Corrélation sur les variables quantitatives
round(cor(subset(poke,select=c(7:11))),2)

####
library(PCAmixdata)
pcamix.temp<- PCAmix(subset(poke,select=c(7:11)) , subset(poke,select=c(3)))
#valeurs propres
print(round(pcamix.temp$eig))
#corrélations
print(round(pcamix.temp$quanti.cor))
#coord. des modalités dudi.mix de ADE4
print(round(pcamix.temp$categ.coord))

#2.3 a)
row.names(poke) <- poke$Name
pcamix.temp<- PCAmix(subset(poke,select=c(7:11)) , subset(poke,select=c(2)))
#valeurs propres
print(round(pcamix.temp$eig))
#corrélations
print(round(pcamix.temp$quanti.cor))
#Dim 1 : Corrélation positive avec toutes les variables 
#Dim 2 : Corrélation negative avec Defense et  positive pour Speed


#coord. des modalités dudi.mix de ADE4
print(round(pcamix.temp$categ.coord))
#2.3 b)
#Pikatchu a les coordonées suivantes
#Pikachu                     -1    1    0    1    0
#On peut donc dire que 
#Ce n'est pas un pokemon "Fort" comparé aux autres (qu'il est moyen partout)
#Il est plutôt rapide
#2.3 c)
#####
poke$X. <- NULL
res <- FAMD(poke)
summary(res)

#2.3 d)
#On peut comparer les pokemons par rapport aux autres et cette fois en intégrants toutes les
#variables, quanti et quali (attention on enlève leurs ID X qui n'est pas une information à utiliser)
#Pour la fonction FAMD je vous laisse regarder dans la doc
help(FAMD)
