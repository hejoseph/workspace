library(nortest)

cidre<-read.csv("cidre.csv",sep=";")


pearson.test(cidre$S.Sucree)
