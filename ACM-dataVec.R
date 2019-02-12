### ACM sur data vectorielle

colnames(dataVec)

noms<-colnames(dataVec[,c(2,14,11,15:32)]) #removed T83 et T85+
valqualis<-dataVec[,noms]
summary(valqualis)

valqualis$T1<- as.factor(valqualis$T1)
valqualis$AgeNum<- as.factor(valqualis$AgeNum)

require(FactoMineR)
require(factoextra)

res.mca<-MCA(valqualis,quali.sup = 1:2)

#6.1% information

### To find correlation between only T8 questions

dataVec->dataVec1
for( i in 24:32){
  dataVec1[,i]<-as.numeric(dataVec1[,i])
}
str(dataVec1)
cor(dataVec1[,24:32])

#install.packages("corrplot")
require(corrplot)
corrplot(cor(dataVec1[,24:32]))

#removing T82+

noms<-colnames(dataVec[,c(2,14,11,15:25)]) #removed T82+
valqualis<-dataVec[,noms]
summary(valqualis)

valqualis$T1<- as.factor(valqualis$T1)
valqualis$AgeNum<- as.factor(valqualis$AgeNum)

res.mca<-MCA(valqualis,quali.sup = 1:2)

#Here we have 4.37% informations. Trying to reduce variables.

#Again correlations

valqualis->valqualis1
for( i in 1:14){
  valqualis1[,i]<-as.numeric(valqualis1[,i])
}
str(valqualis1)
cor00<-cor(valqualis1)

#install.packages("corrplot")
#require(corrplot)
corrplot(cor(cor00))

#T2 and T3 are correlated, as well as T41 and T42, so dropping T3 and T42

valqualis<-valqualis[,-c(5,7)]

###rechecking correlations
valqualis->valqualis1
for( i in 1:12){
  valqualis1[,i]<-as.numeric(valqualis1[,i])
}
str(valqualis1)
cor000<-cor(valqualis1)

#install.packages("corrplot")
#require(corrplot)
corrplot(cor(cor000))

### Continue ACM

res.mca<-MCA(valqualis,quali.sup = 1:2)

###Here he have only 3.98% information kept! Which is terribly low!

dimdesc(res.mca)
summary(res.mca)

plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "contrib 15")
plot.MCA(res.mca,invisible = "ind",cex=0.7)

res.hcpc<-HCPC(res.mca,nb.clust = 3)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")

#Removing temporary variables
rm(i,res.hcpc, res.mca, valqualis, noms, dataVec1, cor00, cor000, valqualis1)
