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


####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec","T41","T42", "T8")), file = "export/classif_questions.RData")

####Suppression de ce qui ne nous sert plus####
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe")))

