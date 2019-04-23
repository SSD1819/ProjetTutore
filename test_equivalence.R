require(effsize)
require(TOSTER)
require(ggplot2)



#### Préparation du jeu de données pour le score global #------------------------------------------------------------------

#La question score_T1 se trouve dans ce RData
load("export/importation.RData")

#Noms des variables de questions (sauf T1) + Pedagogie
selectVar <- c("T21","T22","T23","T31","T32","T41a","T41b","T41c","T41d","T42a",
               "T42b","T42c","T42d","T51","T52","T61",
               "T62","T71","T72","T81","T82","T83","T84",
               "T85","T86","T87","T88","T89","Pedagogie")

#Création du dataFrame qui contient toutes nos questions + pedagogie (et T1 est tranformée en score T1 ici)
dataEquiv <- data.frame(score_T1,dataPropre[,selectVar])

#Renommage de T1 (pour que ça fasse plus propre)
names(dataEquiv)[1] <- "T1"

#Tansformation de nos questions (sans pédagogie) en une matrice NUMÉRIQUE
QMatrix <- matrix(as.numeric(as.matrix(dataEquiv[,-length(names(dataEquiv))])), ncol=length(names(dataEquiv))-1)
colnames(QMatrix) <- names(dataEquiv[,-length(names(dataEquiv))])



#### Calcul du score total (somme des réponses à chaque quetsion) de chaque élève #------------------------------------------------------------------
scoreTot <- apply(QMatrix,1,sum)



#### Graphiques de répartition des scores #------------------------------------------------------------------

#Histogramme de l'ensemble des scores
ggplot(data.frame(scoreTot), aes(x = scoreTot)) +
  geom_histogram(color = "steelblue", fill = "white", bins = 30) +
  xlab("Score total") +
  ylab("Compte")

#Préparation des données pour l'histogramme des scores par pédagogie
scorePedag <- data.frame(scoreTot,dataEquiv$Pedagogie)
names(scorePedag)[2] <- "Pedagogie"

#Histogramme des scores par pédagogie
ggplot(scorePedag, aes(x = scoreTot, color = Pedagogie)) +
  geom_histogram(fill = "white", position = "identity" ,bins = 30) +
  xlab("Score total") +
  ylab("Compte")

#donnees graphique
dataGraphScoreEns <- 
scoreTot <- apply(QMatrix,1,sum)
ggplo()
















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
