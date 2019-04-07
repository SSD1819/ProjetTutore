###Package
require(ClustOfVar)

dataQg.P<-dataPropre[,c(2,13:41)]
dataQSum.P<-dataSumOld[,c(2,14:32)]
dataQVec.P<-dataVecOld[,c(2,14:32)]

T41<-dataQSum.P[,5]
T42<-dataQSum.P[,6]

dataQg.P<-data.frame(dataQg.P[,1:7],T41,T42,dataQg.P[,16:30])



dataQg.P1<-dataQg.P[which(dataQg.P["Pedagogie"]=="Conventionnelle"),]
dataQg.P2<-dataQg.P[which(dataQg.P["Pedagogie"]=="Montessori"),]

dataQSum.P1<-dataQSum.P[which(dataQSum.P["Pedagogie"]=="Conventionnelle"),]
dataQSum.P2<-dataQSum.P[which(dataQSum.P["Pedagogie"]=="Montessori"),]

dataQVec.P1<-dataQVec.P[which(dataQVec.P["Pedagogie"]=="Conventionnelle"),]
dataQVec.P2<-dataQVec.P[which(dataQVec.P["Pedagogie"]=="Montessori"),]




#Affichage des dendogrammes

#######################
########Avec T1
########################

#Pour les données generales avec P1
cahG.P1<-hclustvar(X.quanti=dataQg.P1[,2],X.quali=dataQg.P1[,-c(1,2)])
plot(cahG.P1, main="Dendogramme des donnees generales \n de la pedagogie P1")
rect.hclust(cahG.P1,12)

##Pour les données generales avec P2
cahG.P2<-hclustvar(X.quanti=dataQg.P2[,2],X.quali=dataQg.P2[,-c(1,2)])
plot(cahG.P2, main="Dendogramme des donnees generales\n de la pedagogie P2")
rect.hclust(cahG.P2,12)

##Pour les données avec les vecteurs avec P1
cahVec.P1<-hclustvar(X.quanti=dataQVec.P1[,2],X.quali=dataQVec.P1[,-c(1,2)])
plot(cahVec.P1, main="Dendogramme des données avec les vecteurs\n de la pedagogie P1")
rect.hclust(cahVec.P1,6)

##Pour les données avec les vecteurs avec P2
cahVec.P2<-hclustvar(X.quanti=dataQVec.P2[,2],X.quali=dataQVec.P2[,-c(1,2)])
plot(cahVec.P2, main="Dendogramme des données avec les vecteurs\n de la pedagogie P2")
rect.hclust(cahVec.P2,6)

##Pour les données avec les sommes avec P1
cahSum.P1<-hclustvar(X.quanti=dataQSum.P1[,-c(1,5,6)],X.quali=dataQSum.P1[,c(5,6)])
plot(cahSum.P1, main="Dendogramme des données somme \n de la pedagogie P1")
rect.hclust(cahSum.P1,6)

##Pour les données avec les sommes avec P2
cahSum.P2<-hclustvar(X.quanti=dataQSum.P2[,-c(1,5,6)],X.quali=dataQSum.P2[,c(5,6)])
plot(cahSum.P2, main="Dendogramme des données avec somme\n de la pedagogie P2")
rect.hclust(cahSum.P2,6)


#######################
########Sans T1
#######################
# 
# ##Pour les données generales avec P1
# cahG.P1<-hclustvar(X.quali=dataQg.P1[,-c(1,2)])
# plot(cahG.P1, main="Dendogramme des donnees generales \n de la pedagogie P1")
# rect.hclust(cahG.P1,12)
# 
# ##Pour les données generales avec P2
# cahG.P2<-hclustvar(X.quali=dataQg.P2[,-c(1,2)])
# plot(cahG.P2, main="Dendogramme des donnees generales\n de la pedagogie P2")
# rect.hclust(cahG.P2,12)
# 
# ##Pour les données avec les vecteurs avec P1
# cahVec.P1<-hclustvar(X.quali=dataQVec.P1[,-c(1,2)])
# plot(cahVec.P1, main="Dendogramme des données avec les vecteurs\n de la pedagogie P1")
# rect.hclust(cahVec.P1,6)
# 
# ##Pour les données avec les vecteurs avec P2
# cahVec.P2<-hclustvar(X.quali=dataQVec.P2[,-c(1,2)])
# plot(cahVec.P2, main="Dendogramme des données avec les vecteurs\n de la pedagogie P2")
# rect.hclust(cahVec.P2,6)
# 
# ##Pour les données avec les sommes avec P1
# cahSum.P1<-hclustvar(X.quanti=dataQSum.P1[,-c(1,2,5,6)],X.quali=dataQSum.P1[,c(5,6)])
# plot(cahSum.P1, main="Dendogramme des données avec sommes \n de la pedagogie P1")
# rect.hclust(cahSum.P1,6)
# 
# ##Pour les données avec les sommes avec P2
# cahSum.P2<-hclustvar(X.quanti=dataQSum.P2[,-c(1,2,5,6)],X.quali=dataQSum.P2[,c(5,6)])
# plot(cahSum.P2, main="Dendogramme des données avec sommes\n de la pedagogie P2")
# rect.hclust(cahSum.P2,6)

####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")), file = "export/Classif_Pedagogie.RData")

rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))


