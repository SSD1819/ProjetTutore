### ACM sur data vectorielle
if (!require("FactoMineR")) install.packages("FactoMineR")
require(FactoMineR)
if (!require("factoextra")) install.packages("factoextra")
require(factoextra)
if (!require("dplyr")) install.packages("dplyr")
require(dplyr) 

colnames(dataVec)

noms<-colnames(dataVec[,c(2,14,11,15:25)])
valqualis<-dataVec[,noms]
summary(valqualis)

# valqualis$T1<- as.factor(valqualis$T1)
# valqualis$AgeNum<- as.factor(valqualis$AgeNum)

valqualis<-rbind(valqualis[!rownames(valqualis)%in%"4-2012-049",],valqualis["4-2012-049",])
valqualis<-valqualis[!rownames(valqualis)%in%"4-2012-049",]
res.mca.vec<-MCA(valqualis,quali.sup = 1,quanti.sup = 2:3)

#9.2% information

dimdesc(res.mca.vec)
summary(res.mca.vec)

plot.MCA(res.mca.vec,invisible = "ind",cex=0.7,selectMod =  "contrib 15")
plot.MCA(res.mca.vec,invisible = "ind",cex=0.7)

#pas d'interprétation dessus donc inutile ? 
# res.hcpc<-HCPC(res.mca.vec,nb.clust = 3)
# res.hcpc$desc.var
# plot.HCPC(res.hcpc,choice = "bar")
# 
# valqualisP1<-filter(valqualis, Pedagogie == "P1")
# valqualisP1<-valqualisP1[,-1]
# 
# valqualisP2<-filter(valqualis, Pedagogie == "P2")
# valqualisP2<-valqualisP1[,-1]
# 
# par(mfrow=c(1,2))
# res.mca1<-MCA(valqualisP1,quali.sup = 1:2)
# plot.MCA(res.mca1,choix="ind",invisible = c("ind","quali.sup"))
# dimdesc(res.mca1)
# res.mca2<-MCA(valqualisP2,quali.sup = 1:2)
# plot.MCA(res.mca2,choix="ind",invisible = c("ind","quali.sup"))
#Removing temporary variables and layouts
# par(mfrow=c(1,1))

save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")), file = "export/ACM-datavec.RData")

rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe")))