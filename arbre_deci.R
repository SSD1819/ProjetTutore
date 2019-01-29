# install.packages("tree")
require(tree)

#Remplacement des NAs par une valeur "NA" comme une modalité pour l'arbre
dataTree<-dataPropre
# dataTree[is.na(dataTree)]<-"NoRep"

#Premier arbre de décision pour voir
summary(dataTree)
#Experimentateur+Classe+Type.de.classe+Sexe..F.ou.M.+Langues+Lateralite+Classe.d.age+T21.TOTAL+T22.TOTAL+T23.TOTAL+T31TOTAL+T32.TOTAL+T41a.TOTAL+T41b.TOTAL+T41cTOTAL+T41d.TOTAL+T42a.TOTAL+T42b.TOTAL+T42c.TOTAL+T42d.TOTAL+T51.TOTAL+T52.TOTAL+T61.TOTAL+T62TOTAL+T71.TOTAL+T72TOTAL+T81.TOTAL+T82.TOTAL+T83.TOTAL+T84.TOTAL+T85.TOTAL+T86.TOTAL+T87.TOTAL+T88.TOTAL+T89.TOTAL+T91.Total+T92.Total+T111.TOTAL+T112.TOTAL+T113.TOTAL+annee.scolaire
firstTree<-tree(Pédagogie ~T81.TOTAL+T82.TOTAL,dataTree)
plot(firstTree)
text(firstTree)

data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
summary(cpus)
plot(cpus.ltr);  text(cpus.ltr)
###########
###########
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)# Pour l’arbre de décision
library(rpart.plot) # Pour la représentation de l’arbre de décision