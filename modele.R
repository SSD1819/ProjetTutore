# install.packages("nlme")
require(nlme)
quest<-c("AgeInt","T1", "T21","T22","T23","T31","T32",
         "T41c","T41b","T41a","T41d","T42a","T42b","T42c","T42d",
         "T51","T52","T61","T62","T71","T72" ,             
         "T81","T82","T83","T84",
         "T85","T86","T87","T88",
         "T89","AnneeScolaire","newClasse")
don.reg<-dataPropre[,c("Pedagogie",quest)]

###regression logistique
require(glmm)
# install.packages("doParallel")
# require(doParallel)
# clust<-makeCluster(2)

reg.glmm<-glmm(T21 ~ 0 + Pedagogie + AgeInt + AnneeScolaire,
               random = list(~ 0 + newClasse),
               varcomps.names=c("newClasse"),data=don.reg,
               #the first column reports the number of succeses and the second reports the number of failures
               family.glmm = binomial.glmm)

# stopCluster(clust)
summary(reg.glmm)


###regression linéaire
don.groupe$newClasse<-dataPropre$newClasse
don.groupe$age<-dataPropre$AgeNum
don.groupe$annee<-dataPropre$AnneeScolaire


data<-groupedData(audela~Pedagogie|newClasse, data=don.groupe)
res<-lme(audela~Pedagogie+age, random=~+1|newClasse, data=data)

summary(res)

data<-groupedData(objet~Pedagogie|newClasse, data=don.groupe)
res<-lme(objet~Pedagogie+age, random=~+1|newClasse, data=data)

summary(res)

data<-groupedData(outils~Pedagogie|newClasse, data=don.groupe)
res<-lme(outils~Pedagogie+age, random=~+1|newClasse, data=data)

summary(res)

don.groupe$T1<-dataPropre$T1
data<-groupedData(T1~Pedagogie|newClasse, data=don.groupe)
res<-lme(T1~age, random=~+1|newClasse, data=data)

summary(res)
