colnames(dataPropre)
noms<-c("T23","T31","T52","T62","T86","T87","T88","T89","T42a","T42b","T42c","T42d")
audela<-rowSums(apply(dataPropre[,noms],2,as.numeric))#somme de chaque question concerné par la var audela
audela<-audela+ifelse(dataPropre$T1>7,1,0)#ajout de la t1 si ils savent compter au dela de 7

noms<-c("T41a","T41b","T41c","T41d","T51","T61")
outils<-rowSums(apply(dataPropre[,noms],2,as.numeric))

noms<-c("T21","T22","T32","T81","T82","T83",
       "T84","T85")
objet<-rowSums(apply(dataPropre[,noms],2,as.numeric))

#var pédagogie à comparer avec ces groupes
#voir si une cah montre les mêmes groupes

####
####chi2 2 à 2 sur au dela
####
noms<-c("T23","T31","T52","T62","T86","T87","T88","T89","T42a","T42b","T42c","T42d")
res.chi2<-matrix(nrow = length(noms),ncol = length(noms))
colnames(res.chi2)<-noms
row.names(res.chi2)<-noms
for (i in 1:length(noms)){
  for (j in 1:length(noms)){
    res.chi2[i,j]<-chisq.test(table(dataPropre[,noms[i]],dataPropre[,noms[j]]))$p.value
  }
  
}
ifelse(res.chi2>10**-5,"0","1")
#chi2 significatif entre T21 T22, T22 T32, T84 T22, T84 T21, T83 T22, T85 T32, et toutes les T8


##création de la variable classe sur la t1
Classe_T1<-cut(dataPropre$T1,breaks = c(-1,3,7,11,16,29,100))
levels(Classe_T1)<-c("0-3","4-7","8-11","12-16","17-29",">29")
Classe_T1
don.groupe<-data.frame(Pedagogie=dataPropre$Pedagogie,Classe_T1,audela,outils,objet)
summary(don.groupe)


###regression sur les nvx groupes###
# reg<-glm(Pedagogie~.,data = don.groupe,family = binomial)
# step(reg) ##on ne garde que la regression avec le regroupement "au dela" les autres ne sont pas significative
reg<-glm(Pedagogie~audela,data = don.groupe,family = binomial)
summary(reg)##audela significatif avec la pédagogie

##validation de notre regression
if (!require("ResourceSelection")) install.packages("ResourceSelection")
require(ResourceSelection)
hoslem.test(don.groupe$Pedagogie,fitted(reg))#pvalue significative : modèle non adequate

#réalisation du modèle sur un échantillon
train.id<-sample(seq_len(nrow(don.groupe)),size = 109)
don.train<-don.groupe[train.id,]
don.test<-don.groupe[-train.id,]
reg<-glm(Pedagogie~Classe_T1,data = don.train,family = binomial)
summary(reg)#modèle non significatif 



rm(reg,noms,outils,audela,res.chi2,i,don.test,don.train,train.id)

