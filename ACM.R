colnames(dataPropre)

nom<-colnames(dataPropre[,c(2,11,13:41)])
valquali<-dataPropre[,nom]

summary(valquali)
require(FactoMineR)
require(factoextra)

res.mca<-MCA(valquali,quali.sup = 1,quanti.sup = 2:3)
dimdesc(res.mca)
summary(res.mca)
plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "contrib 15")
plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "cos2 10")
plot.MCA(res.mca,choix = "var",cex=0.7,axes = c(1,2))
plot.MCA(res.mca,choix = "ind",invisible="ind",cex=0.7,axes = c(1,2))#effet gutman !
#
plot.MCA(res.mca,invisible = "ind",cex=0.7,axes = c(1,2))

plot.MCA(res.mca,choix = "ind",cex=0.7,invisible = "ind")
res.hcpc<-HCPC(res.mca,nb.clust = 3)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")


####Clustering de la question 1
res.hcpc<-HCPC(res.mca,nb.clust = 10)
table(res.hcpc$data.clust$T1,res.hcpc$data.clust$clust)
#semble y avoir sur T1 les groupes suivants : 3 - 4 - 5 - 10 - 16 - 20

rm(res.hcpc, res.mca, valquali, nom)



