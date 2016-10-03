read.csv2("ozone.csv")->ozone

# 1er exemple T15
subset(ozone,select=-c(obs,T15,vent,pluie))->tab
lm(T15~.,data=ozone)->modele
residuals(modele)->res
shapiro.test(res)
#p-value > 5%, suit loi normale?

# 2e exemple maxO3
ozone[,c(2:(length(ozone)-2))]->tab1
lm(maxO3~.,data=ozone)->modele1

residuals(modele1)->res1

shapiro.test(res1)
# p-value<(5%), ne suit pas loi normale