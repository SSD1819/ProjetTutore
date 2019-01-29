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

#Suppression des variables inutiles hors de ce script
rm(don1516,don1617,don1718,mathsJetons_2015_2016,mathsJetons_2016_2017,mathsJetons_2017_2018,pos1,NbMod,TabMod)

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

rm(i,nom)

#Vérification qu'il n'y ait pas l'incohérence 0 -> 1 (sauf pour les Q4)