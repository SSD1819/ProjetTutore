require(effsize)
require(TOSTER)

#### Pour les variables quali d'abord (TOST)

###T1

#Echantillon global
t1 <- dataPropre$T1

#Ech pédagogie Conventionnelle
t1C <- t1[dataPropre$Pedagogie=="Conventionnelle"]

#Ech pédagogie Montessori
t1M <- t1[dataPropre$Pedagogie=="Montessori"]

#delta d de Cohen
cohen.d(t1,dataPropre$Pedagogie)
dDown <- as.numeric(cohen.d(t1,dataPropre$Pedagogie)$conf.int[1])
dUp <- as.numeric(cohen.d(t1,dataPropre$Pedagogie)$conf.int[2])

TOSTtwo.raw(m1=mean(t1C),m2=mean(t1M),sd1=sd(t1C),sd2=sd(t1M),n1=length(t1C),n2=length(t1M),low_eqbound=dDown, high_eqbound=dUp, alpha = 0.025, var.equal=TRUE)
###Audela

###Outils

###Objet

#deltaDown

#deltaUp

#delta d de Cohen



####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")), file = "export/test_equivalence.RData")

####Suppression de ce qui ne nous sert plus####
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))
