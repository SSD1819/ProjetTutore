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
res.pca<-PCA(valquanti,quali.sup = 1)
plot.PCA(res.pca,choix = "var",select = "contrib 8") # à nouveau les questions 8 ressortent le plus (puis la 3)

# install.packages("corrplot")
library(corrplot)
m.cor<-cor(valquanti[,(ncol(valquanti)-9):ncol(valquanti)])#matrice des corrélations pour les questions 8
corrplot(m.cor,method = "circle")
#grosse corrélation entre les 8* : donc ACP biaisée
#Etonnemment la T1 ne ressort pas comme grosse contrib
#seulement 60% de l'info sur les 4 premières dimensions
plot.PCA(res.pca,choix = "var",select = "contrib 8",axes = c(3,4)) #Dim 3 > T_72
summary(res.pca)


#test de PCA sans la T1 pour peut etre plus d'info
res.pca1<-PCA(valquanti,quali.sup = 1,quanti.sup = 2)





