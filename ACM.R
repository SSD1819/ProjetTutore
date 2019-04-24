nom<-c("Pedagogie", "T1","T21","T22","T23","T31",
       "T32","T41a","T41b","T41c","T41d","T42a",
       "T42b","T42c","T42d","T51","T52","T61",
       "T62","T71","T72","T81","T82","T83",
       "T84","T85","T86","T87","T88","T89")
valquali<-dataPropre[,nom]

summary(valquali)
require(FactoMineR)
require(factoextra)
valquali$T1<- as.factor(valquali$T1)

#ajout ind fictif pour l'aide à l'interprétation
valquali[nrow(valquali)+1,]<-c("Montessori","0",rep(0,28))
rownames(valquali)[nrow(valquali)]<-"ind_0"
valquali[nrow(valquali)+1,]<-c("Montessori","0",rep(1,28))
rownames(valquali)[nrow(valquali)]<-"ind_1"
summary(valquali)


#ACM
res.mca.globale<-MCA(valquali,quali.sup = 1:2,ind.sup = (nrow(valquali)-1):nrow(valquali))
dimdesc(res.mca.globale)
summary(res.mca.globale)
plot.MCA(res.mca.globale,invisible = "ind",cex=0.7, selectMod =  "contrib 15")
plot.MCA(res.mca.globale,invisible = "ind",cex=0.7)
#Dim1 : mauvais - bons
#Dim2 : T4 - T8
plot.MCA(res.mca.globale,choix = "ind",cex=0.7,axes = c(1,2),invisible = c("ind","var"))
#faible séparation entre P1 et P2 selon la dim1, 0 sur dim2 : pas de diff concrête

res.hcpcmca<-HCPC(res.mca.globale,nb.clust = 3)
res.hcpcmca$desc.var
plot.HCPC(res.hcpcmca,choice = "bar")
# C1 : Echec aux q84 85 87 89 88 ... 
# C2 : Réussite aux q84 82 85 .. 22 mais echec aux q42a-d-c-b-a
# C3 : Réussite aux q4 31 32  8

## > 1 classe d'inds "mauvais" 
## > 1 de bons pour les 8 mais mauvais pour les 4
## > 1 de bons

####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")), file = "export/ACM.RData")

####Suppression de ce qui ne nous sert plus####
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe")))



