library(plyr)

#fonction prop.table(table)
TablesUniv<-function(x){
  return(prop.table(table(x)))
}

#prop de toutes les colonnes
analyse.univ<-apply(X=dataMoySec,MARGIN=2,FUN=TablesUniv)

#effectif de toutes les colonnes
analyse.univ.table<-apply(X=dataMoySec,MARGIN=2,FUN=table)

#Representation des types de classe
pie(analyse.univ$Type.de.classe, labels = rownames(analyse.univ$Type.de.classe) )

#Representation des Droitiers et Gauchers
pie(analyse.univ$Lateralite, labels = c("Utilise les deux pour dessiner","Droitier", "Gaucher"))

#Representation des classes d'age 
pie(analyse.univ$Classe.d.age, labels = c("4 ans", "5 ans"))

#Representation des langues parlées pr les élèves
pie(analyse.univ.table$Langues, labels1 = names(analyse.univ.table$Langues))

#Analyse Question T1 "Vous savez compter jusqu'à combien?"
plot(analyse.univ.table$T1.Réponse[order(analyse.univ.table$T1.Réponse)],type="h", xlab="Limite de comptage", ylab="Nombre d'élèves", main="Histogramme ")
analyse.univ.table$T1.Réponse

#Comptes des résultats

apply(X=dataMoySec[,16:48],MARGIN=1,FUN=plyr::count)

names(dataMoySec)
#Represantation des differents types de pédagogies au sein de l'ecole
pie(analyse.univ.table$Pédagogie)
analyse.univ$Pédagogie
#Majorité de P1







rm(TablesUniv,analyse.univ,analyse.univ.table)
