# install.packages("tree")
require(tree)

#Remplacement des NAs par une valeur "NA" comme une modalité pour l'arbre
dataTree<-dataPropre
#Premier arbre de décision pour voir
summary(dataTree)

# install.packages("rpart")
# install.packages("rpart.plot")
require(rpart)# Pour l’arbre de décision
require(rpart.plot) # Pour la représentation de l’arbre de décision

colnames(dataTree)
quest<-c("T21.TOTAL","T22.TOTAL","T23.TOTAL","T31TOTAL",
         "T32.TOTAL","T41a.TOTAL","T41b.TOTAL","T41cTOTAL",
         "T41d.TOTAL","T42a.TOTAL","T42b.TOTAL","T42c.TOTAL",
         "T42d.TOTAL","T51.TOTAL","T52.TOTAL","T61.TOTAL",
         "T62TOTAL","T71.TOTAL","T72TOTAL","T81.TOTAL","T82.TOTAL",
         "T83.TOTAL","T84.TOTAL","T85.TOTAL","T86.TOTAL",
         "T87.TOTAL","T88.TOTAL","T89.TOTAL","T91.Total",
         "T92.Total","T111.TOTAL","T112.TOTAL","T113.TOTAL")
don.reg<-dataTree[,c("Pédagogie",quest)]
don.tree <- rpart(Pédagogie~.,data=don.reg,method= "class", control=rpart.control(minsplit=5,cp=0))

#Affichage du résultat
plot(don.tree, uniform=TRUE, branch=0.5, margin=0.1)
text(don.tree, all=FALSE, use.n=TRUE,cex=0.7)

plotcp(don.tree)
