dataP<-read.csv("afc_tache_menageres.csv",sep=";")
# barplot(t(dataP[,-1]),beside=T,names=dataP$Task , col = c("lightblue",
# "mistyrose"," lightcyan", "lavender "),legend =colnames(dataP [
# ,2:5]))

chisq.test(dataP[,-1])
dataP_test<-dataP[,-1]
dataP_test2 <- dataP_test
dimnames(dataP_test2) <- list(task = dataP$Task,person = colnames(dataP_test))



#pokemon exo

poke<-read.csv("Pokemon.csv", na.strings=c("","NA"), sep =",")
poke <- as.data.frame(poke)
poke$Generation <- as.factor(poke$Generation)
poke.x <- poke[,c(3 ,12 ,13)]
summary(poke)

acmtot  <- dudi.acm(poke.x,scannf=FALSE)
barplot(acmtot$eig)

# assocstats = permet de quantifier la corélation entre variable. J'ai une donnée plutot proche


pcamix.temp <- PCAmix(subset(poke ,select=c(7:11)) , subset(poke ,select=c(3)))
# level component map : Dragon contribue à cette axe, Ice ne contribue à aucune axe(si on aurait prit un autre dimension, il peut etre tres significatif) = pas d'info à retenir sur cette représentation = ne contenter que deux axes ne veut rien dire, il faut prendre les axes qui apportent plus d'infos
# Fairy est proche de Bug. Lorsqu'on fait la classif,  