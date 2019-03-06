colnames(dataPropre)
noms<-c("T23","T31","T52","T62","T86","T87","T88","T89","T42a","T42b","T42c","T42d")
audela<-rowSums(apply(dataPropre[,noms],2,as.numeric))#somme de chaque question concerné par la var audela
audela<-audela+ifelse(dataPropre$T1>7,1,0)#ajout de la t1 si ils savent compter au dela de 7

noms<-c("T41a","T41b","T41c","T41d","T51","T61")
outils<-rowSums(apply(dataPropre[,noms],2,as.numeric))

noms<-c("T21","T22","T32","T81","T82","T83",
       "T84","T85","T86","T87","T88",
       "T89")
objet<-rowSums(apply(dataPropre[,noms],2,as.numeric))

#var pédagogie à comparer avec ces groupes
#voir si une cah montre les mêmes groupes
#faire le chi2 entre les vars du groupe audela 2 à 2

##tes
