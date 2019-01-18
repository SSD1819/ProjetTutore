dataMoySec_clean<-dataMoySec[,colSums(sapply(lapply(dataMoySec,is.na),as.numeric))==0] #Suppression des colonnes avec NA
#NB colonne total : NA non compté comme des NA
dataMoySec_clean<-dataMoySec_clean[,-c(ncol(dataMoySec_clean)-1)]#suppression de cette colonne donc
summary(dataMoySec_clean)
nom<-colnames(dataMoySec_clean)[c(1,2,3,4,9,11:ncol(dataMoySec_clean)-1)]

for (i in 1:ncol(dataMoySec_clean)){
  if (colnames(dataMoySec_clean)[i]%in%nom ){
    dataMoySec_clean[,i]<-as.factor(dataMoySec_clean[,i])
  }
}
dim(valquali)
valquali<-dataMoySec_clean[,c(2,4,ncol(dataMoySec_clean),9,11:ncol(dataMoySec_clean)-1)]
# valquali<-dataMoySec_clean[,c(2,12:ncol(dataMoySec_clean)-1)]
summary(valquali)
library(FactoMineR)
res.mca<-MCA(valquali,quali.sup = 1)
dimdesc(res.mca)
summary(res.mca)
plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "contrib 15",axes = c(3,4))
plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "cos2 10")
plot.MCA(res.mca,choix = "var",cex=0.7,axes = c(3,4))

plot.MCA(res.mca,invisible = "ind",cex=0.7,axes = c(1,2))
plot.MCA(res.mca,invisible = "ind",cex=0.7,axes = c(1,2))

plot.MCA(res.mca,choix = "ind",cex=0.7,invisible = "ind")
res.hcpc<-HCPC(res.mca,nb.clust = 2)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")


#j'ai fait ca sur rstudioa
#deuxieme changement