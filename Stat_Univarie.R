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
barplot(analyse.univ.table$Pedagogie, col = c("blue", "green"), ylab = "Nombre des eleves",
        xlab = "Pedagogies")
analyse.univ$Pedagogie

#Majorité de P1

rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))

