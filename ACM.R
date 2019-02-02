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
plot.MCA(res.mca,invisible = "ind",cex=0.7,axes = c(1,2))

plot.MCA(res.mca,choix = "ind",cex=0.7,invisible = "ind")
res.hcpc<-HCPC(res.mca,nb.clust = 2)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")

rm(res.hcpc, res.mca, valquali, nom)


