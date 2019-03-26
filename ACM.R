nom<-colnames(dataPropre[,c(2,13,14:41)])#suppression de la variable quanti sur l'age (qui ne ressortait pas de toute façon) car problème lors d'ajout d'individus supplémentaire

valquali<-dataPropre[,nom]

summary(valquali)
require(FactoMineR)
require(factoextra)
valquali$T1<- as.factor(valquali$T1)

#ajout ind fictif pour l'aide à l'interprétation
valquali[nrow(valquali)+1,]<-c("P1",0,rep(0,28))
rownames(valquali)[nrow(valquali)]<-"ind_0"
valquali[nrow(valquali)+1,]<-c("P1",0,rep(1,28))
rownames(valquali)[nrow(valquali)]<-"ind_1"
summary(valquali)


#ACM
res.mca<-MCA(valquali,quali.sup = 1:2,ind.sup = 157:158)
dimdesc(res.mca)
summary(res.mca)
plot.MCA(res.mca,invisible = "ind",cex=0.7,selectMod =  "contrib 15")
plot.MCA(res.mca,invisible = "ind",cex=0.7)
#Dim1 : mauvais - bons
#Dim2 : T4 - T8
plot.MCA(res.mca,choix = "ind",cex=0.7,axes = c(1,2),invisible = c("ind","var"))
#faible séparation entre P1 et P2 selon la dim1, 0 sur dim2 : pas de diff concrête

res.hcpc<-HCPC(res.mca,nb.clust = 3)
res.hcpc$desc.var
plot.HCPC(res.hcpc,choice = "bar")
# C1 : Echec aux q84 85 87 89 88 ... 
# C2 : Réussite aux q84 82 85 .. 22 mais echec aux q42a-d-c-b-a
# C3 : Réussite aux q4 31 32  8

## > 1 classe d'inds "mauvais" 
## > 1 de bons pour les 8 mais mauvais pour les 4
## > 1 de bons

####Clustering de la question 1
res.hcpc<-HCPC(res.mca,nb.clust = 10)
table(res.hcpc$data.clust$T1,res.hcpc$data.clust$clust)
#semble y avoir sur T1 les groupes suivants : 3 - 4 - 5 - 10 - 16 - 20

####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")), file = "export/ACM.RData")

####Suppression de ce qui ne nous sert plus####
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe")))



