#Librairies nécéssaires
require(readxl) 
require(dplyr)
require(stringr)

#Moyenne section 2015/2016
mathsJetons_2015_2016 <- read_excel("MathsJetons_2015-2016.xlsx") %>% data.frame(row.names = 2)
don1516 <- mathsJetons_2015_2016[mathsJetons_2015_2016$Niveau == "MSM",]

#Moyenne section 2016/2017
mathsJetons_2016_2017 <- read_excel("MathsJetons_2016-2017.xlsx",sheet = "QualiSsAtyp")
mathsJetons_2016_2017 <- mathsJetons_2016_2017[is.na(mathsJetons_2016_2017[,2]) == FALSE,] %>% data.frame(row.names = 2)
don1617 <- mathsJetons_2016_2017[mathsJetons_2016_2017$Niveau == "MSM",]

#Moyenne section 2017/2018
mathsJetons_2017_2018 <- read_excel("MathsJetons_2017-2018.xlsx") %>% data.frame(row.names = 2)
don1718 <- mathsJetons_2017_2018[mathsJetons_2017_2018$Niveau == "MSM",]

#Moyenne section 2018/2019
don1819 <- read_excel("Jetons2019.xlsx") %>% data.frame(row.names = 2)

#changement de nom des colonnes (pas identique sur don1516 et les autres)
colnames(don1516)[c(1,9,13,46,47,48)] <- c("Experimentateur","Lateralite","age","T111.TOTAL","T112.TOTAL","T113.TOTAL")
colnames(don1617)[c(1,9,13,46,47,48)] <- c("Experimentateur","Lateralite","age","T111.TOTAL","T112.TOTAL","T113.TOTAL")
colnames(don1718)[c(1,9,13,46,47,48)] <- c("Experimentateur","Lateralite","age","T111.TOTAL","T112.TOTAL","T113.TOTAL")
colnames(don1819) <- colnames(don1718)

#Ensemble des moyennes sections 2015/2016/2017/2018/2019
dataMoySec<-cbind(rbind(don1516,don1617,don1718,don1819),c(rep("15/16",length(don1516[,1])),rep("16/17",length(don1617[,1])),rep("17/18",length(don1718[,1])),rep("18/19",length(don1819[,1]))))
colnames(dataMoySec)[49]<-"annee.scolaire"

#Transformation des noms de pédagogie (p1 = Conventionnelle | p2 = Montessori)
dataMoySec$Pédagogie[which(dataMoySec$Pédagogie=="P1"|dataMoySec$Pédagogie=="Traditionnelle")]<-"Conventionnelle"
dataMoySec$Pédagogie[which(dataMoySec$Pédagogie=="P2")]<-"Montessori"

#Recherche des variables à une seule modalité
NbMod<-function(x){
  return(length(table(x)))
}
pos1<-which(apply(X=dataMoySec,MARGIN=2,FUN=NbMod)==1)

#Suppression des variables qui n'ont qu'une modalité et ne servent à rien ("Ecole" et "Niveau")
dataPropre<-dataMoySec[,-pos1]

#Stats univariées vite fait
TabMod<-function(x){
  return(table(x))
}
apply(X=dataPropre,MARGIN=2,FUN=TabMod)

#Correction des modalités de la variable Type.de.classe
dataPropre$Type.de.classe<-str_to_lower(dataPropre$Type.de.classe)
dataPropre$Type.de.classe<-sub("grands","grand",dataPropre$Type.de.classe)
dataPropre$Type.de.classe<-sub("grand","grands",dataPropre$Type.de.classe)

#Correction des modalités de la variable Langues
dataPropre$Langues<-str_to_lower(dataPropre$Langues)
dataPropre$Langues<-sub("/","et",dataPropre$Langues)

#Suppression des variables T11 et T9
dataPropre<-dataPropre[,-c(42:46)]

#Stats univariées sur le jeu de données (table)
apply(X=dataPropre,MARGIN=2,FUN=TabMod)

#Remplacement du nom de la variable "Type.de.classe" par "type.de.classe"
names(dataPropre)[which(names(dataPropre)=="Type.de.classe")]<-"type.de.classe"

#On remplace les na des questions par "0"
posQ<-which(substr(names(dataPropre),1,1)=="T")
questions<-dataPropre[,posQ]
questions<-apply(questions,2,as.numeric)
questions[is.na(questions)]<-0

#Vérification qu'il n'y ait pas l'incohérence 0 -> 1 (sauf pour les Q4)
vecAnte<-t(apply(questions[,c(2:28)],1,as.numeric))
colnames(vecAnte)<-colnames(questions[,c(2:28)])
vecPost<-t(apply(questions[,c(3:29)],1,as.numeric))
colnames(vecPost)<-colnames(questions[,c(3:29)])
vecDiff<-vecPost-vecAnte

#Il existe des incohérences, on les recherche
#On enlève d'abord les questions qui ne peuvent pas avoir d'incohérence
incoher<-vecDiff[,which(as.numeric(substr(colnames(questions[,3:29]),3,3))>1 & as.numeric(substr(colnames(questions[,3:29]),2,2))!=4 & as.numeric(substr(colnames(questions[,3:29]),2,2))!=8)]

#On récupère ensuite les lignes et colonnes comprtant 1 à la diff entre post et ante
incoher2<-incoher[which(apply(incoher,1,max)==1),which(apply(incoher,2,max)==1)]

#Remplacement des questions avec NA par questions avec 0 dans datapropre
dataPropre[,posQ]<-questions

#Changement des variables en facteurs
nom<-c("Experimentateur","Pédagogie","Classe","Type.de.classe",
       "Sexe..F.ou.M.","Langues","Lateralite",
       "Classe.d.age",
       "T21.TOTAL","T22.TOTAL","T23.TOTAL","T31TOTAL","T32.TOTAL",
       "T41a.TOTAL","T41b.TOTAL","T41cTOTAL","T41d.TOTAL","T42a.TOTAL",
       "T42b.TOTAL","T42c.TOTAL","T42d.TOTAL","T51.TOTAL","T52.TOTAL","T61.TOTAL",
       "T62TOTAL","T71.TOTAL","T72TOTAL","T81.TOTAL","T82.TOTAL","T83.TOTAL","T84.TOTAL",
       "T85.TOTAL","T86.TOTAL","T87.TOTAL","T88.TOTAL","T89.TOTAL","annee.scolaire")


for (i in 1:ncol(dataPropre)){
  if (colnames(dataPropre)[i]%in%nom ){
    dataPropre[,i]<-as.factor(dataPropre[,i])
  }
}

#Correction de la question 2 (enfants 0->1 devient 1->1)
dataPropre$T21.TOTAL[which(dataPropre$T21.TOTAL=="0" & dataPropre$T22.TOTAL=="1")]<-"1"

#Création du jeu de données où les résultats des questions sont des vecteurs
T1<-questions[,1]
T2<-paste(questions[,2],questions[,3],questions[,4],sep="")
T3<-paste(questions[,5],questions[,6],sep="")
T41<-paste(questions[,7],questions[,8],questions[,9],questions[,10],sep="")
T42<-paste(questions[,11],questions[,12],questions[,13],questions[,14],sep="")
T5<-paste(questions[,15],questions[,16],sep="")
questionsVec<-cbind(T1,T2,T3,T41,T42,T5,questions[,17:29])
dataVec<-cbind(dataPropre[,-posQ],questionsVec)
dataVec$T1<-T1

#Création du jeu de données où les résultats des question sont des sommes
questionsNum<-apply(questions,2,as.numeric)
T1<-questionsNum[,1]
T2<-questionsNum[,2]+questionsNum[,3]+questionsNum[,4]
T3<-questionsNum[,5]+questionsNum[,6]
T41<-paste(questions[,7],questions[,8],questions[,9],questions[,10],sep="")
T42<-paste(questions[,11],questions[,12],questions[,13],questions[,14],sep="")
T5<-questionsNum[,15]+questionsNum[,16]
questionsSum<-data.frame(T1,T2,T3,T41,T42,T5,questionsNum[,17:29])
dataSum<-cbind(dataPropre[,-posQ],questionsSum)


#####Changement du nom des colonnes qui ne sont pas très propres

names(dataPropre)[c(2,4,5,8,9:13,42)]<-c(
  "Pedagogie",
  "TypeClasse",
  "Sexe",
  "DateNaissance",
  "DateEval",
  "AgeChar",
  "AgeNum",
  "AgeInt",
  "T1",
  "AnneeScolaire"
)

names(dataVec)[c(2,4,5,8,9:13)]<-c(
  "Pedagogie",
  "TypeClasse",
  "Sexe",
  "DateNaissance",
  "DateEval",
  "AgeChar",
  "AgeNum",
  "AgeInt",
  "AnneeScolaire"
)

names(dataSum)[c(2,4,5,8,9:13)]<-c(
  "Pedagogie",
  "TypeClasse",
  "Sexe",
  "DateNaissance",
  "DateEval",
  "AgeChar",
  "AgeNum",
  "AgeInt",
  "AnneeScolaire"
)

#Suppression ". TOTAL"
names(dataPropre)<-sub(".","",names(dataPropre),fixed=TRUE)
names(dataPropre)<-sub("TOTAL","",names(dataPropre),fixed=TRUE)
names(dataPropre)<-sub("Total","",names(dataPropre),fixed=TRUE)
names(dataVec)<-sub(".","",names(dataVec),fixed=TRUE)
names(dataVec)<-sub("TOTAL","",names(dataVec),fixed=TRUE)
names(dataSum)<-sub(".","",names(dataSum),fixed=TRUE)
names(dataSum)<-sub("TOTAL","",names(dataSum),fixed=TRUE)

#Sauvegarde de dataSum et dataVec avant le regroupement de T8.123 et T8.456789
dataSumOld <- dataSum
dataVecOld <- dataVec

#Regroupement des quesions T8.123 et T8.456789 dans le jeu dataVec
cols.123<-c(names(dataVec[,24:26]))
cols.456789<-c(names(dataVec[,27:32]))
T8.123<-apply(dataVec[,cols.123],1, paste , collapse = "" )
T8.456789<-apply(dataVec[,cols.456789],1, paste , collapse = "" )
dataVec<-cbind(dataVec, T8.123, T8.456789)
dataVec <- dataVec[,!(names(dataVec) %in% c(cols.123,cols.456789 )) ]


#Regroupement des quesions T8.123 et T8.456789 dans le jeu dataSum
T8.123<-dataSum[,"T81"]+dataSum[,"T82"]+dataSum[,"T83"]
T8.456789<-dataSum[,"T84"]+dataSum[,"T85"]+dataSum[,"T86"]+dataSum[,"T87"]+dataSum[,"T88"]+dataSum[,"T89"]
dataSum<-cbind(dataSum, T8.123, "T8.456789"=T8.456789)
dataSum<-dataSum[,!(names(dataSum) %in% c(cols.123,cols.456789 )) ]

#### Création des nouvelles variables (au dela, outils, etc...)
noms<-c("T23","T31","T52","T62","T86","T87","T88","T89","T42a","T42b","T42c","T42d")
audela<-rowSums(apply(dataPropre[,noms],2,as.numeric))#somme de chaque question concerné par la var audela
audela<-audela+ifelse(dataPropre$T1>7,1,0)#ajout de la t1 si ils savent compter au dela de 7

noms<-c("T41a","T41b","T41c","T41d","T51","T61")
outils<-rowSums(apply(dataPropre[,noms],2,as.numeric))

noms<-c("T21","T22","T32","T81","T82","T83","T84","T85")
objet<-rowSums(apply(dataPropre[,noms],2,as.numeric))

##création de la variable classe sur la t1
Classe_T1<-cut(dataPropre$T1,breaks = c(-1,3,7,11,16,29,100))
levels(Classe_T1)<-c("0-3","4-7","8-11","12-16","17-29",">29")
Classe_T1
don.groupe<-data.frame(Pedagogie=dataPropre$Pedagogie,Classe_T1,audela,outils,objet)
summary(don.groupe)

#Supression des variables qui ne servent à rien
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))
