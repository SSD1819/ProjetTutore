require(FactoMineR)
require(factoextra)
summary(dataSum)

#soit acp sur tout sauf la Q4 car vectorielle
#soit afm
colnames(dataSum)
noms<-c("Pedagogie", "T1","T2","T3",
        "T5","T61","T62","T71","T72","T81",
        "T82","T83","T84","T85","T86","T87",
        "T88","T89")
valquanti<-dataSum[,noms]
valquanti[,2:length(valquanti)]<-scale(valquanti[,2:length(valquanti)])
res.pca<-PCA(valquanti,quali.sup = 1)
plot.PCA(res.pca,choix = "var",select = "contrib 8") # à nouveau les questions 8 ressortent le plus (puis la 3)

# install.packages("corrplot")
library(corrplot)
m.cor<-cor(valquanti[,2:ncol(valquanti)])#matrice des corrélations pour les questions 8
corrplot(m.cor,method = "circle")
#grosse corrélation entre les 8* : donc ACP biaisée
#Etonnemment la T1 ne ressort pas comme grosse contrib
#seulement 60% de l'info sur les 4 premières dimensions
plot.PCA(res.pca,choix = "var",select = "contrib 8",axes = c(3,4)) #Dim 3 > T_72
summary(res.pca)

##Nouvelle PCA sans les T8 super correlées entre elles (que T81 car non correlée)
valquanti1<-valquanti[,-c(11:18)]
summary(valquanti1)
res.pca1<-PCA(valquanti1,quali.sup = 1)
plot.PCA(res.pca1,axes = c(1,2),choix = "ind")#aucune démarcation entre P1 et P2
plot.PCA(res.pca1,axes = c(1,2),choix = "var",select = "cos2 5")#T2 / T3 proche : gros score en T2 > gros score en T3
#D1 : T2 | T3
#D2 : T62 
plot.PCA(res.pca1,axes = c(3,4),choix = "var")
#D3 : T72
#D4 : T5
#quasi 1 variable / axe > peu utile 

summary(res.pca1)

rm(res.pca,res.pca1,valquanti)

