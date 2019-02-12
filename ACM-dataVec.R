### ACM sur data vectorielle

colnames(dataVec)

noms<-colnames(dataVec[,c(2,14,11,15:23)]) #removed T8
valqualis<-dataVec[,noms]
summary(valqualis)

### To find correlations

valqualis->valqualis1
for( i in 1:12){
  valqualis1[,i]<-as.numeric(valqualis1[,i])
}
str(valqualis1)
cor00<-cor(valqualis1)

#install.packages("corrplot")
require(corrplot)
corrplot(cor(cor00))
###

#T2 and T3 are correlated, as well as T41 and T42, so dropping T3 and T42

valqualis<-valqualis[,-c(5,7)]

###rechecking correlations
valqualis->valqualis1
for( i in 1:10){
  valqualis1[,i]<-as.numeric(valqualis1[,i])
}
str(valqualis1)
cor000<-cor(valqualis1)

#install.packages("corrplot")
#require(corrplot)
corrplot(cor(cor000))

### Continue ACM

require(FactoMineR)
require(factoextra)
valqualis$T1<- as.factor(valqualis$T1)
valqualis$AgeNum<- as.factor(valqualis$AgeNum)

res.mca<-MCA(valqualis,quali.sup = 1:2)

###Here he have only 4.14% information kept! Which is terribly low!

dimdesc(res.mca)
summary(res.mca)

plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "contrib 15")
plot.MCA(res.mca,invisible = "ind",cex=0.7)

res.hcpc<-HCPC(res.mca,nb.clust = 3)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")

#Removing temporary variables
rm(i,res.hcpc, res.mca, valqualis, noms, dataVec1, cor00, cor000, valqualis1)
