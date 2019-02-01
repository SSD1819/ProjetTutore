######
#SUR VECTEUR
######
summary(dataVec)

# install.packages("rpart")
# install.packages("rpart.plot")
require(rpart)# Pour l’arbre de décision
require(rpart.plot) # Pour la représentation de l’arbre de décision

colnames(dataVec)
quest<-c("Sexe","AgeInt","T1","T2","T3",
         "T41","T42","T5","T6","T7",             
         "T81","T82","T83","T84",
         "T85","T86","T87","T88",
         "T89")
don.reg<-dataVec[,c("Pedagogie",quest)]
don.tree <- rpart(Pedagogie~Sexe+AgeInt+T1+T2,data=don.reg)
#Affichage du résultat
plot(don.tree, uniform=TRUE, branch=0.5, margin=0.1)
text(don.tree, all=FALSE, use.n=TRUE,cex=0.7)

plotcp(don.tree)

reg<-glm(Pedagogie~T3+T2,data=don.reg)

tree.opt<-prune(don.tree,cp=don.tree$cptable[which.min(don.tree$cptable[,4]),1])#
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

