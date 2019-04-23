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
  geom_histogram(aes(y=..density..), color = "steelblue", fill = "white", binwidth = 1) +
  geom_density(alpha=0.2, fill="#FF6666")+
  xlab("Score total") +
  ylab("Densité")

#Préparation des données pour l'histogramme des scores par pédagogie
scorePedag <- data.frame(scoreTot,dataEquiv$Pedagogie)
names(scorePedag)[2] <- "Pedagogie"

#Histogramme des scores par pédagogie
ggplot(scorePedag, aes(x = scoreTot, color = Pedagogie, fill = Pedagogie)) +
  geom_histogram(aes(y=..density..), position = "identity" ,binwidth = 1, alpha = 0.2) +
  geom_density(alpha = 0.5)+
  xlab("Score total") +
  ylab("Compte")

#### Test d'equivalence sur le score total #------------------------------------------------------------------

#Echantillon pédagogie conventionnelle
scoreTotConv <- scoreTot[dataEquiv$Pedagogie=="Conventionnelle"]

#Echantillon pédagogie Montessori
scoreTotMont <- scoreTot[dataEquiv$Pedagogie=="Montessori"]

#dDown et dUp
dUp <- (2/20)*29
dDown <- 0-dUp

#Test de comparaison de variances
var.test(scoreTotConv,scoreTotMont) #Variance égale à priori

#Le test
TOSTtwo.raw(m1=mean(scoreTotConv),m2=mean(scoreTotMont),sd1=sd(scoreTotConv),sd2=sd(scoreTotMont),n1=length(scoreTotConv),n2=length(scoreTotMont),low_eqbound=dDown, high_eqbound=dUp, alpha = 0.025, var.equal=TRUE)


####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")), file = "export/test_equivalence.RData")

####Suppression de ce qui ne nous sert plus####
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))
