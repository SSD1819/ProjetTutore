library(plyr)

#fonction prop.table(table)
TablesUniv<-function(x){
  return(prop.table(table(x)))
}

#prop de toutes les colonnes
analyse.univ<-apply(X=dataPropre,MARGIN=2,FUN=TablesUniv)

#effectif de toutes les colonnes
analyse.univ.table<-apply(X=dataPropre,MARGIN=2,FUN=table)

#Representation des Droitiers et Gauchers
pie(analyse.univ$Lateralite, labels = c("Utilise les deux pour dessiner","Droitier", "Gaucher"))

#Representation des langues parlées pr les élèves
pie(analyse.univ.table$Langues, labels1 = names(analyse.univ.table$Langues))

#Analyse Question T1 "Vous savez compter jusqu'à combien?"
plot(analyse.univ.table$T1[order(analyse.univ.table$T1)],type="h", 
     xlab="Limite de comptage", ylab="Nombre d'élèves", main="Histogramme ")
analyse.univ.table$T1

#Comptes des résultats

apply(X=dataPropre[,16:42],MARGIN=1,FUN=plyr::count)

names(dataPropre)

#Represantation des differents types de pédagogies au sein de l'ecole
pie(analyse.univ.table$Pedagogie)
analyse.univ$Pédagogie

#Majorité de P1

#Resultats univarie en comparant les 2 pedagogies
require(dplyr)
dataPropreP1<-filter(dataPropre, Pedagogie == "P1")
dataPropreP2<-filter(dataPropre, Pedagogie == "P2")

#As numeric questions T2, T3 et T5
for (i in c(14:18,27,28)){
dataPropreP1[,i]<-as.numeric(levels(dataPropreP1[,i]))[dataPropreP1[,i]]
}

for (i in c(14:18,27,28)){
  dataPropreP2[,i]<-as.numeric(levels(dataPropreP2[,i]))[dataPropreP2[,i]]
}

par(mfrow=c(1,2))
boxplot(dataPropreP1$T1, main="T1 Reponses (P1)")
boxplot(dataPropreP2$T1, main="T1 Reponses (P2)")

prop.table(table(dataPropreP1[,14]))*100 #T21 Reponses (P1)
prop.table(table(dataPropreP2[,14]))*100 #T21 Reponses (P2)
pie(table(dataPropreP1[,14]), main="T21 Reponses (P1)",col = c("orange","green"))
pie(table(dataPropreP2[,14]), main="T21 Reponses (P2)",col = c("orange","green"))

prop.table(table(dataPropreP1[,15]))*100 #T22 Reponses (P1)
prop.table(table(dataPropreP2[,15]))*100 #T22 Reponses (P2)
pie(table(dataPropreP1[,15]), main="T22 Reponses (P1)",col = c("orange","green"))
pie(table(dataPropreP2[,15]), main="T22 Reponses (P2)",col = c("orange","green"))

prop.table(table(dataPropreP1[,16]))*100 #T23 Reponses (P1)
prop.table(table(dataPropreP2[,16]))*100 #T23 Reponses (P2)
pie(table(dataPropreP1[,16]), main="T23 Reponses (P1)",col = c("orange","green"))
pie(table(dataPropreP2[,16]), main="T23 Reponses (P2)",col = c("orange","green"))

prop.table(table(dataPropreP1[,17]))*100 #T31 Reponses (P1)
prop.table(table(dataPropreP2[,17]))*100 #T31 Reponses (P2)
pie(table(dataPropreP1[,17]), main="T31 Reponses (P1)",col = c("orange","green"))
pie(table(dataPropreP2[,17]), main="T31 Reponses (P2)",col = c("orange","green"))

prop.table(table(dataPropreP1[,18]))*100 #T32 Reponses (P1)
prop.table(table(dataPropreP2[,18]))*100 #T32 Reponses (P2)
pie(table(dataPropreP1[,18]), main="T32 Reponses (P1)",col = c("orange","green"))
pie(table(dataPropreP2[,18]), main="T32 Reponses (P2)",col = c("orange","green"))

prop.table(table(dataPropreP1[,27]))*100 #T51 Reponses (P1)
prop.table(table(dataPropreP2[,27]))*100 #T51 Reponses (P2)
pie(table(dataPropreP1[,27]), main="T51 Reponses (P1)",col = c("orange","green"))
pie(table(dataPropreP2[,27]), main="T51 Reponses (P2)",col = c("orange","green"))

prop.table(table(dataPropreP1[,28]))*100 #T52 Reponses (P1)
prop.table(table(dataPropreP2[,28]))*100 #T52 Reponses (P2)
pie(table(dataPropreP1[,28]), main="T52 Reponses (P1)",col = c("orange","green"))
pie(table(dataPropreP2[,28]), main="T52 Reponses (P2)",col = c("orange","green"))

par(mfrow=c(1,1))
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")))

