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

colnames(dataPropre)
quest<-c("Sexe","AgeInt","T1", "T21","T22","T23","T31","T32",
         "T41c","T41b","T41a","T41d","T42a","T42b","T42c","T42d",
         "T51","T52","T61","T62","T71","T72" ,             
         "T81","T82","T83","T84",
         "T85","T86","T87","T88",
         "T89")
summary(dataPropre)
don.reg<-dataPropre[,c("Pedagogie",quest)]

#essaie de passer par la reg logistique pour une cp normale
reg<-glm(Pedagogie~T22+T41c+T42c+T71+T81+T89,data=don.reg,family = binomial(link = "logit"))
reg<-glm(Pedagogie~T41c+T89,data=don.reg,family = binomial(link = "logit"))#T42c limite
# reg<-glm(Pedagogie~.,data=don.reg,family = binomial)
summary(reg)

exp(-0.6028+-0.7822*0+1.0380)/(1+exp(-0.6028+-0.7822*0+1.0380))

plotcp(rpart(reg))

don.tree <- rpart(reg)
#Affichage du résultat
plot(don.tree, uniform=TRUE, branch=0.5, margin=0.1)
text(don.tree, all=FALSE, use.n=TRUE,cex=0.7)

plotcp(don.tree)

tree.opt<-prune(don.tree,cp=0.01)#
prp(tree.opt,type=4,extra = 1)

rm(don.tree,reg,don.reg,tree.opt)

