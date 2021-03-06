####
####chi2 2 à 2 sur au dela
####
noms<-c("T23","T31","T42a","T42b","T42c","T42d","T52","T62","T86","T87","T88","T89")
res.chi2<-matrix(nrow = length(noms),ncol = length(noms))
colnames(res.chi2)<-noms
row.names(res.chi2)<-noms
for (i in 1:length(noms)){
  for (j in 1:length(noms)){
    res.chi2[i,j]<-chisq.test(table(dataPropre[,noms[i]],dataPropre[,noms[j]]))$p.value
  }
  
}
ifelse(res.chi2>10**-5,"0","1")
#chi2 significatif entre T21 T22, T22 T32, T84 T22, T84 T21, T83 T22, T85 T32, et toutes les T8



###regression sur les nvx groupes###
# reg<-glm(Pedagogie~.,data = don.groupe,family = binomial)
# step(reg) ##on ne garde que la regression avec le regroupement "au dela" les autres ne sont pas significative
reg.nul<-glm(Pedagogie~audela,data = don.groupe,family = binomial)
summary(reg.nul)##Aucune variable ne ressort de la regression logistique pour la pédagogie

##validation de notre regression
if (!require("ResourceSelection")) install.packages("ResourceSelection")

hoslem.test(don.groupe$Pedagogie,fitted(reg.nul))#pvalue significative : modèle non adequate

if (!require("pROC")) install.packages("pROC")
#prediction des données grâce à la regression sur le meme échantillon
reg.pred.nul<-predict.glm(reg.nul,type = "response")
tt<-as.factor(ifelse(reg.pred.nul<0.5,"Conventionnelle","Montessori"))
sum(tt==dataPropre$Pedagogie)/nrow(dataPropre)#71% de bonne prédiction

reg.link.nul<-predict(reg.nul, type="link")

reg.roc.nul<-roc(response = dataPropre$Pedagogie, predictor = reg.pred.nul, direction="<",auc = TRUE)
#AUC = 77%

plot.roc(reg.roc.nul,col="blue", lwd=3)

#réalisation du modèle sur un échantillon
train.id<-sample(seq_len(nrow(don.groupe)),size = 109)
don.train<-don.groupe[train.id,]
don.test<-don.groupe[-train.id,]
reg.nul<-glm(Pedagogie~Classe_T1,data = don.train,family = binomial)
summary(reg.nul)#modèle non significatif 

##

#essaie de passer par la reg logistique pour une cp normale



###Affinage de la variable "audela"
T1bin<-ifelse(dataPropre$T1>7,1,0)
reg.nul<-glm(Pedagogie~T23+T31+T52+T62+T86+T87+T88+T89+T42a+T42b+T42c+T42d, data=dataPropre,family = binomial)
step(reg.nul)


###

reg.nul<-glm(formula = Pedagogie ~ T41b + T41c + T1bin, family = binomial, data = dataPropre)
summary(reg.nul)


simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}


if (!require(pROC)) install.packages("pROC")
#prediction des données grâce à la regression sur le meme échantillon
reg.pred.nul<-predict.glm(reg.nul,type = "response")
tt<-as.factor(ifelse(reg.pred.nul<0.5,"P1","P2"))
sum(tt==dataPropre$Pedagogie)/nrow(dataPropre)#71% de bonne prédiction

reg.link.nul<-predict(reg, type="link")

reg.roc.nul<-roc(response = dataPropre$Pedagogie, predictor = reg.pred.nul, direction="<",auc = TRUE)
#AUC = 77%

plot.roc(reg.roc.nul,col="yellow", lwd=3)
glm_simple_roc.nul <- simple_roc(dataPropre$Pedagogie=="P2", reg.link.nul)
with(glm_simple_roc, points(1 - FPR, TPR, col=1 + labels, cex = 0.7))



####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")), file = "export/New_Variables.RData")

rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))
