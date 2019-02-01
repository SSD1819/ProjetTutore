####Préparation jeu de données pour les questions####

#On enlève tout ce qui n'a pas de rapport avec les questions ainsi que la T11
dataQg<-dataPropre[,13:43]
dataQSum<-dataSum[,14:30]
dataQVec<-dataVec[,14:30]

#Split de la Q4 en Q41 et Q42
T41<-substr(dataSum$T4,1,4)
T42<-substr(dataSum$T4,5,8)

#Remplacement de la Q4 par Q41 et Q42
dataQg<-data.frame(dataQg[,1:6],T41,T42,dataQg[,15:31])
dataQSum<-data.frame(dataQSum[,1:3],T41,T42,dataQSum[,5:17])
dataQVec<-data.frame(dataQVec[,1:3],T41,T42,dataQVec[,5:17])


####Classification (T1 Quali)####
require(ClustOfVar)

###CAH

#Pour les donnees générales
cahG<-hclustvar(X.quanti=dataQg[,1],X.quali=dataQg[,-1])
plot(cahG)
rect.hclust(cahG,12)

#Pour les données vec
cahVec<-hclustvar(X.quanti=dataQVec[,1],X.quali=dataQVec[,-1])
plot(cahVec)
rect.hclust(cahVec,6)

#Pour les données somme
cahSum<-hclustvar(X.quanti=dataQSum[,-c(4,5)],X.quali=dataQSum[,4:5])
plot(cahSum)
rect.hclust(cahSum,6)

###Kmeans (marche pas encore)
#testk<-kmeansvar(X.quanti=dataQuestions[,1],X.quali=dataQuestions[,-1],rename.level=TRUE)

rm(dataQg,dataQSum,dataQVec,T41,T42,cahG,cahVec,cahSum)
