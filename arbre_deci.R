######
#SUR VECTEUR
######
summary(dataVec)

#Plusieurs prob : reg logistique que sur variables binaire / quanti : donc inutile de passer en vect
#ne comprend pas le std error
#ne comprend pas pk l'erreur augmente quand la complexité de l'arbre augmente et qu'elle est aléatoire (pas logique) (et que le modele est satisfaisant)
#



# install.packages("rpart")
# install.packages("rpart.plot")
require(rpart)# Pour l’arbre de décision
require(rpart.plot) # Pour la représentation de l’arbre de décision

colnames(dataVec)
quest<-c("Sexe","AgeInt","T1", "T21","T22","T23","T3","T32",
         "T41","T41d","T42a","T42b","T42c","T42d",
         "T51","T52","T61","T6","T71","T7" ,             
         "T81","T82","T83","T84",
         "T85","T86","T87","T88",
         "T89")
summary(dataPropre)
don.reg<-dataPropre[,c("Pedagogie",quest)]

#essaie de passer par la reg logistique pour une cp normale
reg<-glm(Pedagogie~T1+T41+T89,data=don.reg,family = binomial)
reg<-glm(Pedagogie~.,data=don.reg,family = binomial)
summary(reg)
plotcp(rpart(reg))

don.tree <- rpart(reg)
#Affichage du résultat
plot(don.tree, uniform=TRUE, branch=0.5, margin=0.1)
text(don.tree, all=FALSE, use.n=TRUE,cex=0.7)

plotcp(don.tree)

tree.opt<-prune(don.tree,cp=0.01)#
prp(tree.opt,extra = 1)

######
#SUR SCORE
######
summary(dataSum)#Aucun score en quantitatif ? et encore des NA dans la T11
colnames(dataSum)
quest<-c("Sexe","Classe.d.age","T1","T2","T3",
         "T4","T5","T6","T7",             
         "T81","T82","T83","T84",
         "T85","T86","T87","T88",
         "T89")
don.reg<-dataSum[,c("Pédagogie",quest)]
don.tree <- rpart(Pédagogie~.,data=don.reg,method= "class", control=rpart.control(minsplit=1,cp=0))
#Affichage du résultat
plot(don.tree, uniform=TRUE, branch=0.5, margin=0.1)
text(don.tree, all=FALSE, use.n=TRUE,cex=0.7)
# prp(don.tree,extra = 1)
plotcp(don.tree)

tree.opt<-prune(don.tree,cp=don.tree$cptable[which.min(don.tree$cptable[,4]),1])#
prp(tree.opt,extra = 1)

