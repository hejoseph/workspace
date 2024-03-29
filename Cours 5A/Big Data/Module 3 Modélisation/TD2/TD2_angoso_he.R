library(tree)

# TD2 : Arbres de d�cision

# Bin�me : Stefan Angoso et Joseph HE

data <- read.csv2 ("Salaries.csv", sep = ",", header = TRUE)

# Variable inint�ressante ici
data$X <- NULL

salaire <- as.data.frame(data)

summary (salaire)

# En qualitatif, on remarque qu'il y a beaucoup plus de profs, d'hommes, et que la discipline B est l�g�rement plus repr�sent�e que la A
# En quantitatif, 



# 1.2 Package Tree

tree.Lin <- tree (salary ~ yrs.service + yrs.since.phd, data = salaire )
plot(tree.Lin)
text(tree.Lin, cex=.75)

# a) Anciennet� comprise entre 32 et 45.5, qui a �t� moins de 21.5 ans dans le m�me service

# b) Nous avons g�n�r� 9 feuilles, donc 9 classes

# c)
tree.model <- tree(log(salary) ~ yrs.service + yrs.since.phd, data = salaire)

# d)
plot(tree.model)
text(tree.model,cex=0.75)

# Pourquoi ce ne sont pas les plus anciens qui ont les meilleurs salaires ?
# Attention, on n'a pas pris la discipiline
# ex : informatique r�cente mais bien pay�e

# e)
# On voit mieux les diff�rences pour les salaires proches, on voit mieux la limite des extr�mes sur la m�me �chelle



# 
salar.deciles <- quantile (salaire$salary, 0:10/10)
cut.prices <- cut (salaire$salary,salar.deciles,include.lowest = TRUE )
plot (salaire$yrs.service , salaire$yrs.since.phd , col = grey(10:2/11)[cut.prices], pch =20, xlab ="yrs.service", ylab ="yrs.since.phd")

partition.tree(tree.model,ordvars = c("yrs.service","yrs.since.phd"), add = TRUE )

plot(salaire$yrs.since.phd, salaire$salary, pch=19, col = as.numeric(salaire$rank ))
partition.tree (tree.model, label ="Species", add = TRUE)
legend("topright", legend = unique(salaire$rank), col = unique(as.numeric(salaire$rank)), pch=19)


# attention au seuil de split du tree


# b) On remarque des diff�rences de salaire et de repr�sentativit� des individus.
# Par exemple, les nouveaux arrivants (moins de 10 ans PhD) et ont un salaire faible mais attention, la probabilit� de les avoir est faible aussi (couleur claire).
# De m�me, les plus anciens sont peu repr�sent�s.


# c) On remarque que les nouveaux arrivants sont au rang d'assistants (entre 0 et 8 ans).  Leur salaire est relativement faible.
# Ensuite, on voit entre 8 et 14 ans, on a une grande concentration d'associ�s. Le salaire est un peu meilleur que pour les assistants en g�n�ral.
# Et enfin, � partir de 14 ans, on trouve �norm�ment de professeurs (quelques associ�s mais c'est rare). On trouve des salaires � hauteur des associ�s mais �galement des salaires tr�s hauts (200k)


tree.model2 <- tree (log(salary) ~ yrs.service + yrs.since.phd, data = salaire, mindev =0.001)
plot (tree.model2)
text (tree.model2, cex =.75)

# a) On a beaucoup plus de noeuds et donc de feuilles que pr�c�demment

# b) On a chang� la d�viance minimale, ici � 0.001

# c) Le treemodel2 est beaucoup plus pr�cis car il a beaucoup plus de classes, mais il est plus d�pendant de l'�chantillon et du bruit, et par opposition le treemodel1 est plus parcimonieux.

# d) L'arbre le plus performant est treemodel1 d'un point de vue pr�dictif car il a tendance � mieux g�n�raliser et donc ne pas surapprendre.

# e) 

pruned.tree <- prune.tree(tree.model2, best = 4)
plot(pruned.tree)
text(pruned.tree)


# f)

pruned.tree2 <- prune.tree(tree.model2, best = 4)
plot(pruned.tree2)
text(pruned.tree2)

# g) Le meilleur arbre est


# h)



# 2 Les arbres de classification

# 2.1 Chargement des donn�es

data <- read.csv2 ("titanic.csv", sep = ",",header = TRUE )

titanic <- as.data.frame(data)

summary(titanic)

# a) X n'est qu'un ID et donc pas analysable quantitativement. Il y a simplement 1316 personnes en tout.
# On remarque qu'il y a beaucoup plus de classes 3 (54%), donc de personnes plus "pauvres", que ce qu'il y a dans les classes 2 et 1 r�unies. 
# On trouve majoritairement des adultes (92%), mais attention, � cette �poque on pouvait �tre consid�r� adulte tr�s t�t (10-12 ans)
# On trouve �galement plus d'hommes (66%) que de femmes 
# b) On met la colonne � NULL
titanic$X<-NULL

# c) Age, Sexe et Survived sont binaires, Class a trois modalit�s

library("rpart")
library("rpart.plot")
library("party")

str(titanic)

# 2.2 Cr�ation de l'arbre

myFormula <- survived ~ class + age

fit <- rpart (myFormula , method = "class", data = titanic)
titanic_tree <- ctree(myFormula, data = titanic)

# a) On cherche � atteindre un maximum de puret� dans les noeuds-fils, donc � r�duire l'indice de Gini. Gini(pere) - Gini(fils) est donc positif

# b) On obtient des r�sultats similaires, 4 

# c) Nous n'avons pas besoin d'�laguer car on utilise un test statistique pour �valuer la pertinence des s�parations successives. Je cite la documentation "This statistical approach ensures that the right sized tree is grown and no form of pruning or cross-validation or whatsoever is needed"

# d) Ctree utilise un test statistique pour choisir ses variables, alors que CART choisit les variables qui maximisent un crit�re (celui de Gini), et n�cessite une cross-validation ult�rieure.

# e) fit2 et titanic_tree2
form <- survived ~ class + age + sex
fit2 <- rpart(form, method = "class", data = titanic)

plot (fit2, uniform = TRUE )
text (fit2, use.n=TRUE, all=TRUE)

titanic_tree2 <- ctree (form, data = titanic )
plot (titanic_tree2, uniform = TRUE )
text (titanic_tree2, use.n=TRUE, all=TRUE)


# 2.3 Am�lioration de l'arbre

predictions1 <- predict (titanic_tree, titanic)
table(titanic$survived, predictions1)

predictions2 <- predict (titanic_tree2, titanic)
table(titanic$survived, predictions2)

# a) On remarque que les deux mod�les sont aussi mauvais l'un que l'autre pour les yes : un yes sur deux se retrouve injustement en no.
# En revanche, pour les no, predictions2 est bien bien meilleur : il ne s'est tromp� que 17 fois, alors que predictions 1 s'est tromp� 122 fois.


library (randomForest)
r <- randomForest(survived ~., data = titanic, importance =TRUE, do.trace =100, ntree =100)

predictionsr <- predict (r, titanic)
table (predictions2, predictionsr)
plot(r)
legend("topright", colnames(r$err.rate),col=1:3,cex=0.8,fill=1:3)

# b) Nous obtenons sensiblement les m�mes r�sultats entre titanic_tree2 et r

# c) En rouge, nous voyons l'erreur sur les no, en vert l'erreur sur les yes et en noir l'out of bag error, l'erreur de pr�diction moyenne

# d) Non, elles ne peuvent pas faire de randomForest