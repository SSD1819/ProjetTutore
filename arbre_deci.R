require(tree)

#Remplacement des NAs par une valeur "NA" comme une modalité pour l'arbre
dataTree<-dataPropre
dataTree[is.na(dataTree)]<-"NoRep"

#Premier arbre de décision pour voir
firstTree<-tree(dataTree$Pédagogie~.)
plot(firstTree)
text(firstTree)