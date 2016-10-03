data<-read.csv2("Salaries.csv",sep=",",header=TRUE)
data$X<-NULL
salaire<-as.data.frame(data)
summary(data)

library(tree)
tree.Lin<-tree(salary~yrs.service+yrs.since.phd,data=salaire)
#1.2.c
tree.model<-tree(log(salary)~yrs.service+yrs.since.phd,data=salaire)
plot(tree.model)
text(tree.model,cex=.75)


#1.3
salar.deciles<-quantile(salaire$salary,0:10/10)
cut.prices<-cut(salaire$salary,salar.deciles,include.lowest=TRUE)
plot(salaire$yrs.service,salaire$yrs.since.phd,col=grey(10:2/11)[cut.prices],pch=20,xlab="yrs.service",ylab="yrs.since.phd")
####Partition
partition.tree(tree.model,ordvars=c("yrs.service","yrs.since.phd"),add=TRUE)
#Figure5
plot(salaire$yrs.since.phd,salaire$salary,pch=19,col=as.numeric(salaire$rank))
partition.tree(tree.model,label="Species",add=TRUE)
legend("topright",legend=unique(salaire$rank),col=unique(as.numeric(salaire$rank)),pch=19)


summary(tree.model)



###Nouveaumodel
tree.model2<-tree(log(salary)~yrs.service+yrs.since.phd,
data=salaire,mindev=0.001)
plot(tree.model2)
text(tree.model2,cex=.75)
summary(tree.model2)

#

#Elagation
pruned.tree<-prune.tree(tree.model,best=4)
plot(pruned.tree)
text(pruned.tree)




#chargerlesdonnees-attentionauxoptions
data<-read.csv2("titanic.csv",sep=",",header=TRUE)
titanic<-as.data.frame(data)
###Summary
summary(titanic)

titanic$X<-NULL

View(data)

###Installeretcharger
library(rpart)
library(party)
library(rpart.plot)
str(titanic)

#if fisher pvalue <5%, then we can separate class

myFormula<-survived~class+age#
##CART##
fit<-rpart(myFormula,method="class",data=titanic)
printcp(fit)#displaytheresults


plotcp(fit)#visualizecross-validationresult(Figure8)
summary(fit)#detailedsummaryofsplits

#critere de jinny, s'il tend vers 0 plus de ressemblance

#plottreeFigure9
plot(fit,uniform=TRUE)
text(fit,use.n=TRUE,all=TRUE,cex=.7)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

#anova : quali en entré, et quanti en sortie

#Onameliorelegraphiquequin"estpastreslisible:Figure10
fancyRpartPlot(fit)
##CTREE##
titanic_tree<-ctree(myFormula,data=titanic)
print(titanic_tree)



##Figure11
plot(titanic_tree)



predictions<-predict(titanic_tree,titanic)
table(titanic$survived,predictions)


predictions<-predict(titanic_tree2,titanic)
table(titanic$survived,predictions)


library(randomForest)
r<-randomForest(survived~.,data=titanic,importance=TRUE,do.
trace=100,ntree=100)


plot(r)
predictions<-predict(r,titanic)
table(titanic$survived,predictions)


print(r)
