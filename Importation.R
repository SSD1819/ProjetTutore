#Librairies nécéssaires
require(readxl)
require(dplyr)
require(stringr)

#Moyenne section 2015/2016
mathsJetons_2015_2016 <- read_excel("MathsJetons_2015-2016.xlsx") %>% data.frame(row.names = 2)
don1516 <- mathsJetons_2015_2016[mathsJetons_2015_2016$Niveau == "MSM",]
#don1516<-don1516[,!(colnames(don1516)%in%c("Expérimentateur","Type de classe","Langues","Latéralité"))]

#Moyenne section 2016/2017
mathsJetons_2016_2017 <- read_excel("MathsJetons_2016-2017.xlsx",sheet = "QualiSsAtyp")
mathsJetons_2016_2017 <- mathsJetons_2016_2017[is.na(mathsJetons_2016_2017[,2]) == FALSE,] %>% data.frame(row.names = 2)
don1617 <- mathsJetons_2016_2017[mathsJetons_2016_2017$Niveau == "MSM",]
#don1617<-don1617[,!(colnames(don1617)%in%c("Expérimentateur","Type de classe","Langues","Latéralité"))]

#Moyenne section 2017/2018
mathsJetons_2017_2018 <- read_excel("MathsJetons_2017-2018.xlsx") %>% data.frame(row.names = 2)
don1718<-mathsJetons_2017_2018[mathsJetons_2017_2018$Niveau == "MSM",]
#don1718<-don1718[,!(colnames(don1718)%in%c("Expérimentateur","Type de classe","Langues","Latéralité"))]

#changement de nom des colonnes (pas identique sur don1516 et les autres)
colnames(don1516)[c(1,9,13,46,47,48)] <- c("Experimentateur","Lateralite","age","T111.TOTAL","T112.TOTAL","T113.TOTAL")
colnames(don1617)[c(1,9,13,46,47,48)] <- c("Experimentateur","Lateralite","age","T111.TOTAL","T112.TOTAL","T113.TOTAL")
colnames(don1718)[c(1,9,13,46,47,48)] <- c("Experimentateur","Lateralite","age","T111.TOTAL","T112.TOTAL","T113.TOTAL")

#Ensemble des moyennes sections 2015/2016/2017/2018
dataMoySec<-cbind(rbind(don1516,don1617,don1718),c(rep("15/16",length(don1516[,1])),rep("16/17",length(don1617[,1])),rep("17/18",length(don1718[,1]))))
colnames(dataMoySec)[49]<-"annee.scolaire"

#Recherche des variables à une seule modalité
NbMod<-function(x){
  return(length(table(x)))
}
pos1<-which(apply(X=dataMoySec,MARGIN=2,FUN=NbMod)==1)

#Suppression des variables qui n'ont qu'une modalité et ne servent à rien ("Ecole" et "Niveau")
dataPropre<-dataMoySec[,-pos1]

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

#Transformation des "NA" en vrais NA
dataPropre$T111.TOTAL[which(dataPropre$T111.TOTAL=="NA")]<-NA
dataPropre$T112.TOTAL[which(dataPropre$T112.TOTAL=="NA")]<-NA
dataPropre$T113.TOTAL[which(dataPropre$T113.TOTAL=="NA")]<-NA

apply(X=dataPropre,MARGIN=2,FUN=TabMod)

#Changement des variables en facteurs
nom<-c("Experimentateur","Pédagogie","Classe","Type.de.classe",
       "Sexe..F.ou.M.","Langues","Lateralite",
       "Classe.d.age",
       "T21.TOTAL","T22.TOTAL","T23.TOTAL","T31TOTAL","T32.TOTAL",
       "T41a.TOTAL","T41b.TOTAL","T41cTOTAL","T41d.TOTAL","T42a.TOTAL",
       "T42b.TOTAL","T42c.TOTAL","T42d.TOTAL","T51.TOTAL","T52.TOTAL","T61.TOTAL",
       "T62TOTAL","T71.TOTAL","T72TOTAL","T81.TOTAL","T82.TOTAL","T83.TOTAL","T84.TOTAL",
       "T85.TOTAL","T86.TOTAL","T87.TOTAL","T88.TOTAL","T89.TOTAL","T91.Total","T92.Total",
       "T111.TOTAL","T112.TOTAL","T113.TOTAL","annee.scolaire")


for (i in 1:ncol(dataPropre)){
  if (colnames(dataPropre)[i]%in%nom ){
    dataPropre[,i]<-as.factor(dataPropre[,i])
  }
}

dataPropre$T1.Réponse<-as.numeric(dataPropre$T1.Réponse)
summary(dataPropre)

#Remplacement du nom de la variable "Type.de.classe" par "type.de.classe"
names(dataPropre)[which(names(dataPropre)=="Type.de.classe")]<-"type.de.classe"

#On remplace les na des questions par "0"
posQ<-which(substr(names(dataPropre),1,1)=="T" & substr(names(dataPropre),1,3)!="T11")
questions<-dataPropre[,posQ]
questions[is.na(questions)]<-"0"

#Vérification qu'il n'y ait pas l'incohérence 0 -> 1 (sauf pour les Q4)
vecAnte<-t(apply(questions[,c(2:30)],1,as.numeric))
colnames(vecAnte)<-names(questions[,c(2:30)])
vecPost<-t(apply(questions[,c(3:31)],1,as.numeric))
colnames(vecPost)<-names(questions[,c(3:31)])
vecDiff<-vecPost-vecAnte

#Il existe des incohérences, on les recherche
#On enlève d'abord les questions qui ne peuvent pas avoir d'incohérence
incoher<-vecDiff[,which(as.numeric(substr(names(questions[,3:31]),3,3))>1 & as.numeric(substr(names(questions[,3:31]),2,2))!=4 & as.numeric(substr(names(questions[,3:31]),2,2))!=8)]

#On récupère ensuite les lignes et colonnes comprtant 1 à la diff entre post et ante
incoher2<-incoher[which(apply(incoher,1,max)==1),which(apply(incoher,2,max)==1)]

#Remplacement des questions avec NA par questions avec 0 dans datapropre
dataPropre[,posQ]<-questions

#Suppression de T11 dans datapropre
dataPropre<-dataPropre[,-c(44:46)]

#Création du jeu de données où les résultats des questions sont des vecteurs
T1<-questions[,1]
T2<-paste(questions[,2],questions[,3],questions[,4],sep="")
T3<-paste(questions[,5],questions[,6],sep="")
T41<-paste(questions[,7],questions[,8],questions[,9],questions[,10],sep="")
T42<-paste(questions[,11],questions[,12],questions[,13],questions[,14],sep="")
T5<-paste(questions[,15],questions[,16],sep="")
T6<-paste(questions[,17],questions[,18],sep="")
T7<-paste(questions[,19],questions[,20],sep="")
T9<-paste(questions[,30],questions[,31],sep="")
questionsVec<-cbind(T1,T2,T3,T41,T42,T5,T6,T7,questions[,21:29],T9)
dataVec<-cbind(dataPropre[,-posQ],questionsVec)

#Création du jeu de données où les résultats des question sont des sommes
questionsNum<-apply(questions,2,as.numeric)
T1<-questionsNum[,1]
T2<-questionsNum[,2]+questionsNum[,3]+questionsNum[,4]
T3<-questionsNum[,5]+questionsNum[,6]
T41<-paste(questions[,7],questions[,8],questions[,9],questions[,10],sep="")
T42<-paste(questions[,11],questions[,12],questions[,13],questions[,14],sep="")
T5<-questionsNum[,15]+questionsNum[,16]
T6<-questionsNum[,17]+questionsNum[,18]
T7<-questionsNum[,19]+questionsNum[,20]
T9<-questionsNum[,30]+questionsNum[,31]
questionsSum<-data.frame(T1,T2,T3,T41,T42,T5,T6,T7,questionsNum[,21:29],T9)
dataSum<-cbind(dataPropre[,-posQ],questionsSum)

#####Changement du nom des colonnes qui ne sont pas très propres

names(dataPropre)[c(2,4,5,8,9:13,44)]<-c(
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
names(dataPropre)<-sub("cTOTAL","c",names(dataPropre))
names(dataPropre)<-sub(".TOTAL","",names(dataPropre))
names(dataPropre)<-sub(".Total","",names(dataPropre))
names(dataVec)<-sub(".TOTAL","",names(dataVec))
names(dataSum)<-sub(".TOTAL","",names(dataSum))

#Supression des variables qui ne servent à rien
rm(dataMoySec,incoher,questions,questionsNum,questionsSum,questionsVec,vecAnte,vecDiff,vecPost,posQ,T1,T2,T3,T41,T42,T5,T6,T7,T9,don1516,don1617,don1718,mathsJetons_2015_2016,mathsJetons_2016_2017,mathsJetons_2017_2018,pos1,NbMod,TabMod,i,nom)
