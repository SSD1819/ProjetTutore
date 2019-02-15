### ACM sur data vectorielle
if (!require("FactoMineR")) install.packages("FactoMineR")
require(FactoMineR)
if (!require("factoextra")) install.packages("factoextra")
require(factoextra)

colnames(dataVec)

noms<-colnames(dataVec[,c(2,14,11,15:25)])
valqualis<-dataVec[,noms]
summary(valqualis)

valqualis$T1<- as.factor(valqualis$T1)
valqualis$AgeNum<- as.factor(valqualis$AgeNum)

res.mca<-MCA(valqualis,quali.sup = 1:3)

#9.2% information

dimdesc(res.mca)
summary(res.mca)

plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "contrib 15")
plot.MCA(res.mca,invisible = "ind",cex=0.7)

res.hcpc<-HCPC(res.mca,nb.clust = 3)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")

library(dplyr)    
valqualisP1<-filter(valqualis, Pedagogie == "P1")
valqualisP1<-valqualisP1[,-1]

valqualisP2<-filter(valqualis, Pedagogie == "P2")
valqualisP2<-valqualisP1[,-1]

par(mfrow=c(1,2))
res.mca1<-MCA(valqualisP1,quali.sup = 1:2)
res.mca2<-MCA(valqualisP2,quali.sup = 1:2)

#Removing temporary variables
rm(res.hcpc, res.mca, valqualis, noms, 
   res.mca1,res.mca2, valqualisP1,valqualisP2)
