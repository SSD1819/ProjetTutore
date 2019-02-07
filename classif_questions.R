####Préparation jeu de données pour les questions####

#On enlève tout ce qui n'a pas de rapport avec les questions
dataQg<-dataPropre[,13:41]
dataQSum<-dataSum[,14:32]
dataQVec<-dataVec[,14:32]

#Regroupement Q41 et Q42 (a b c d)
T41<-dataQSum[,4]
T42<-dataQSum[,5]

#Remplacement de la Q4 par Q41 et Q42
dataQg<-data.frame(dataQg[,1:6],T41,T42,dataQg[,15:29])

#Jeu de données où la Q8 est regroupée
#Vec
T8<-paste(dataQSum[,11],dataQSum[,12],dataQSum[,13],dataQSum[,14],dataQSum[,15],dataQSum[,16],dataQSum[,17],dataQSum[,18],dataQSum[,19],sep="")
dataQVec8<-cbind(dataQVec[,1:10],T8)
#Sum
T8<-dataQSum[,11]+dataQSum[,12]+dataQSum[,13]+dataQSum[,14]+dataQSum[,15]+dataQSum[,16]+dataQSum[,17]+dataQSum[,18]+dataQSum[,19]
dataQSum8<-cbind(dataQSum[,1:10],T8)


####Classification (T1 Quanti)####
require(ClustOfVar)

###Pour les donnees générales###
cahG<-hclustvar(X.quanti=dataQg[,1],X.quali=dataQg[,-1])
plot(cahG, main="Clust des données générales")
rect.hclust(cahG,12)


###Pour les données vec###
cahVec<-hclustvar(X.quanti=dataQVec[,1],X.quali=dataQVec[,-1])
plot(cahVec, main="Clust des données vec")
rect.hclust(cahVec,6)

#Q8 regroupée
cahVec8<-hclustvar(X.quanti=dataQVec8[,1],X.quali=dataQVec8[,-1])
plot(cahVec8, main="Clust des données vec avec Q8 regroupée")
rect.hclust(cahVec8,6)


###Pour les données somme###
cahSum<-hclustvar(X.quanti=dataQSum[,-c(4,5)],X.quali=dataQSum[,4:5])
plot(cahSum, main="Clust des données somme")
rect.hclust(cahSum,6)

#Q8 regroupée
cahSum8<-hclustvar(X.quanti=dataQSum8[,-c(4,5)],X.quali=dataQSum8[,4:5])
plot(cahSum8, main="Clust des données somme avec 8 regroupée")
rect.hclust(cahSum8,6)


####Classification (sans T1)####

###Pour les donnees générales###
cahGWO1<-hclustvar(X.quali=dataQg[,-1])
plot(cahGWO1, main="Clust des données générales sans T1")
rect.hclust(cahGWO1,10)


###Pour les données vec###
cahVecWO1<-hclustvar(X.quali=dataQVec[,-1])
plot(cahVecWO1, main="Clust des données vec sans T1")
rect.hclust(cahVecWO1,6)

#Q8 regroupée
cahVec8WO1<-hclustvar(X.quali=dataQVec8[,-1])
plot(cahVec8WO1, main="Clust des données vec sans T1 avec Q8 regroupée")
rect.hclust(cahVec8WO1,6)


###Pour les données somme###
cahSumWO1<-hclustvar(X.quanti=dataQSum[,-c(1,4,5)],X.quali=dataQSum[,4:5])
plot(cahSumWO1, main="Clust des données somme sans T1")
rect.hclust(cahSumWO1,6)

#Q8 regroupée
cahSum8WO1<-hclustvar(X.quanti=dataQSum8[,-c(1,4,5)],X.quali=dataQSum8[,4:5])
plot(cahSum8WO1, main = "Clust des données somme sans T1 avec Q8 regroupée")
rect.hclust(cahSum8WO1,6)







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
plot(cahG.P1, main="Donnees generales avec P1")
rect.hclust(cahG.P1,12)

###Donnees generales avec P2
cahG.P2<-hclustvar(X.quanti=dataQg.P2[,2],X.quali=dataQg.P2[,-c(1,2)])
plot(cahG.P2, main="Donnees generales avec P2")
rect.hclust(cahG.P2,12)

###Pour les données vec avec P1
cahVec.P1<-hclustvar(X.quanti=dataQVec.P1[,2],X.quali=dataQVec.P1[,-c(1,2)])
plot(cahVec.P1, main="Clust des données vec avec P1")
rect.hclust(cahVec.P1,6)

###Pour les données vec avec P2
cahVec.P2<-hclustvar(X.quanti=dataQVec.P2[,2],X.quali=dataQVec.P2[,-c(1,2)])
plot(cahVec.P2, main="Clust des données vec avec P2")
rect.hclust(cahVec.P2,6)

###Pour les données somme avec P1
cahSum.P1<-hclustvar(X.quanti=dataQSum.P1[,-c(1,5,6)],X.quali=dataQSum.P1[,c(5,6)])
plot(cahSum.P1, main="Clust des données somme avec P1")
rect.hclust(cahSum.P1,6)

###Pour les données somme avec P2
cahSum.P2<-hclustvar(X.quanti=dataQSum.P2[,-c(1,5,6)],X.quali=dataQSum.P2[,c(5,6)])
plot(cahSum.P2, main="Clust des données somme avec P2")
rect.hclust(cahSum.P2,6)



rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")))

