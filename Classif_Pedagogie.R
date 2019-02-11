
######Jeux de données pour les clusters avec Pedagogie ######
dataQg.P<-dataPropre[,c(2,13:41)]
dataQSum.P<-dataSum[,c(2,14:32)]
dataQVec.P<-dataVec[,c(2,14:32)]


T41<-dataQSum.P[,5]
T42<-dataQSum.P[,6]

#Remplacement de la Q4 par Q41 et Q42
dataQg.P<-data.frame(dataQg.P[,1:7],T41,T42,dataQg.P[,16:30])

#Jeu de données où la Q8 est regroupée
#Vec
T8.P<-paste(dataQSum.P[,12],dataQSum.P[,13],dataQSum.P[,14],dataQSum.P[,15],dataQSum.P[,16],dataQSum.P[,17],dataQSum.P[,18],dataQSum.P[,19],dataQSum.P[,20],sep="")
dataQVec8.P<-cbind(dataQVec.P[,2:11],T8.P)
#Sum
T8.P<-dataQSum.P[,12]+dataQSum.P[,13]+dataQSum.P[,14]+dataQSum.P[,15]+dataQSum.P[,16]+dataQSum.P[,17]+dataQSum.P[,18]+dataQSum.P[,19]+dataQSum.P[,20]
dataQSum8.P<-cbind(dataQSum.P[,1:10],T8.P)

### Séparation P1 et P2 ###
dataQg.P1<-dataQg.P[which(dataQg.P["Pedagogie"]=="P1"),]
dataQg.P2<-dataQg.P[which(dataQg.P["Pedagogie"]=="P2"),]

dataQSum.P1<-dataQSum.P[which(dataQSum.P["Pedagogie"]=="P1"),]
dataQSum.P2<-dataQSum.P[which(dataQSum.P["Pedagogie"]=="P2"),]

dataQVec.P1<-dataQVec.P[which(dataQVec.P["Pedagogie"]=="P1"),]
dataQVec.P2<-dataQVec.P[which(dataQVec.P["Pedagogie"]=="P2"),]




### Pour les donnees générales ###
cahG<-hclustvar(X.quanti=dataQg.P[,2],X.quali=dataQg.P[,-2])
plot(cahG)
rect.hclust(cahG,12)

### Donnees generales avec P1
cahG.P1<-hclustvar(X.quanti=dataQg.P1[,2],X.quali=dataQg.P1[,-c(1,2)])
plot(cahG.P1, main="Dendogramme des donnees generales \n avec P1")
rect.hclust(cahG.P1,12)

###Donnees generales avec P2
cahG.P2<-hclustvar(X.quanti=dataQg.P2[,2],X.quali=dataQg.P2[,-c(1,2)])
plot(cahG.P2, main="Dendogramme des donnees generales\n avec P2")
rect.hclust(cahG.P2,12)

###Pour les données vec avec P1
cahVec.P1<-hclustvar(X.quanti=dataQVec.P1[,2],X.quali=dataQVec.P1[,-c(1,2)])
plot(cahVec.P1, main="Dendogramme des données vec\n avec P1")
rect.hclust(cahVec.P1,6)

###Pour les données vec avec P2
cahVec.P2<-hclustvar(X.quanti=dataQVec.P2[,2],X.quali=dataQVec.P2[,-c(1,2)])
plot(cahVec.P2, main="Dendogramme des données vec\n avec P2")
rect.hclust(cahVec.P2,6)

###Pour les données somme avec P1
cahSum.P1<-hclustvar(X.quanti=dataQSum.P1[,-c(1,5,6)],X.quali=dataQSum.P1[,c(5,6)])
plot(cahSum.P1, main="Dendogramme des données somme\n avec P1")
rect.hclust(cahSum.P1,6)

###Pour les données somme avec P2
cahSum.P2<-hclustvar(X.quanti=dataQSum.P2[,-c(1,5,6)],X.quali=dataQSum.P2[,c(5,6)])
plot(cahSum.P2, main="Dendogramme des données somme\n avec P2")
rect.hclust(cahSum.P2,6)

rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")))
