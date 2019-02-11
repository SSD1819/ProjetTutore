
### PAS FINIT ###

### ACM sur data vectorielle

colnames(dataVec)

noms<-colnames(dataVec[,c(2,14,11,15:25,27)]) #removed T83 et T85+
valqualis<-dataVec[,noms]
summary(valqualis)

### To find correlation between questions T8

dataVec->dataVec1
for( i in 24:32){
  dataVec1[,i]<-as.numeric(dataVec1[,i])
}
str(dataVec1)
cor(dataVec1[,24:32])

#install.packages("corrplot")
require(corrplot)
corrplot(cor(dataVec1[,24:32]))

###

### Continue ACM

require(FactoMineR)
require(factoextra)
valqualis$T1<- as.factor(valqualis$T1)
valqualis$AgeNum<- as.factor(valqualis$AgeNum)


res.mca<-MCA(valqualis,quali.sup = 1:3)

dimdesc(res.mca)
summary(res.mca)

plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "contrib 15")
plot.MCA(res.mca,invisible = "ind",cex=0.7)

res.hcpc<-HCPC(res.mca,nb.clust = 3)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")


# rm(res.hcpc, res.mca, valqualis, noms, dataVec1)



