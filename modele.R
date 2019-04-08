# install.packages("nlme")
require(nlme)
quest<-c("AgeInt","T1", "T21","T22","T23","T31","T32",
         "T41c","T41b","T41a","T41d","T42a","T42b","T42c","T42d",
         "T51","T52","T61","T62","T71","T72" ,             
         "T81","T82","T83","T84",
         "T85","T86","T87","T88",
         "T89","AnneeScolaire","newClasse")
don.reg<-dataPropre[,c("Pedagogie",quest)]
###regression linéaire



###regression logistique
data<-groupedData(log(Y+0.5)~Condition|N_repl, data=dataPropre)
res<-lme(log(Y+0.5)~Condition+DIV, random=~1, data=data, na.action=na.omit)


modele logistique à effets aléatoires : package glmm


