d<-read.csv("MyData.csv",sep=",")

mine.avg=mean(d$mine)
imdb.avg=mean(d$imdb)

p1<-ggplot(d,aes(x=mine))+
geom_density(alpha=0.5,aes(x=mine,y=..density..,fill="blue"))+
geom_density(alpha=0.5,aes(x=imdb,y=..density..,fill="red"))+
geom_histogram(aes(y=count/sum(..count..)))+
geom_vline(xintercept=mine.avg,color="blue")+
geom_vline(xintercept=imdb.avg,color="red")+
scale_x_continuous("IMDbratings",breaks=seq(2,10,1))+
scale_y_continuous("Density")+
theme_bw()+theme(legend.position="none")

summary(m1<-lm(mine~imdb,data=d))
ml2<-lm(formula=mine~imdb,data=d)

p2<-ggplot(d,aes(imdb,mine))+
geom_point(position=position_jitter(width=0.1,height=.25),shape=16,size=4,alpha=0.6,aes(colour=new.genre,))+
stat_smooth(se=TRUE)+
scale_x_continuous("IMDbratings")+
scale_y_continuous("Persoratings")+
theme_bw()+
scale_colour_discrete(name="Genre")+
scale_size_continuous(guide=FALSE)+
theme(legend.position=c(0.15,0.80))+
geom_abline(size=1,aes(intercept=-0.6387,slope=0.9686))

#Ona baissé notre erreur quadratique moyenne : donc modele meilleure
# M2 = c'est votre modèle ou vous avez réalisé ...

# un modèle de régression linéaire n'est pas forcémment une droite, on veut prédire approximativement, intervalle de confiance

predlims<-function(preds,sigma){
	prediction.sd<-sqrt(preds$se.fit^2+sigma^2)
	upper<-preds$fit+2*prediction.sd
	lower<-preds$fit-2*prediction.sd
	lims<-cbind(lower=lower,upper=upper)
	return(lims)
}

preds.lm<-predict(m1,se.fit=TRUE)
predlims.lm<-predlims(preds.lm,sigma=summary(m1)$sigma)

mean(d$mine<=predlims.lm[,"upper"]&d$mine>=predlims.lm[,"lower"])

plot(d$mine,preds.lm$fit,type="n",xlim=c(2,10),ylim=c(2,10),
xlab="Myactualratings",ylab="Predictedratings",main="")

segments(d$mine,predlims.lm[,"lower"],
d$mine,predlims.lm[,"upper"],col="grey")

abline(a=0,b=1,lty="dashed")

points(d$mine,preds.lm$fit,pch=16,cex=0.8)


# On a deux façon : 
# Soit on ajoute des param en plus
# Soit on restreint notre apprentissage 
# pb de densité : pk biaise le model ? car user give good mark and not bad 

# au début, on intègre param, on regarde comment ça s'améliore






