######
#SUR VECTEUR
######

if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")

quest<-c("AgeInt","T1", "T21","T22","T23","T31","T32",
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
reg<-glm(Pedagogie~.,data=don.reg,family = binomial)
step(reg)

reg<-glm(formula = Pedagogie ~ T21 + T22 + T31 + T41b + T51 + T81 + 
           T83 + T89, family = binomial, data = don.reg)
summary(reg)

#rajouter des interaction + step + courbe roc
#refaire avec score en quali

##test de hosmer lemershow ##
if (!require("ResourceSelection")) install.packages("ResourceSelection")
hoslem.test(don.reg$Pedagogie,fitted(reg))

plotcp(rpart(reg,method = "class"))

don.tree <- rpart(reg)
#Affichage du résultat
plot(don.tree, uniform=TRUE, branch=0.5, margin=0.1)
text(don.tree, all=FALSE, use.n=TRUE,cex=0.7)

plotcp(don.tree)

tree.opt<-prune(don.tree,cp=0.01)#
prp(tree.opt,type=4,extra = 1)


#####
#####
#COURBE ROC
#####
#####

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}


if (!require(pROC)) install.packages("pROC")
#prediction des données grâce à la regression sur le meme échantillon
reg.pred<-predict.glm(reg,type = "response")
tt<-as.factor(ifelse(reg.pred<0.5,"Conventionnelle","Montessori"))
sum(tt==dataPropre$Pedagogie)/nrow(dataPropre)#64% de bonne prédiction

reg.link<-predict(reg, type="link")

reg.roc<-roc(response = dataPropre$Pedagogie, predictor = reg.pred, direction="<",auc = TRUE)
#AUC = 77%

plot.roc(reg.roc,col="yellow", lwd=3)
glm_simple_roc <- simple_roc(dataPropre$Pedagogie=="Montessori", reg.link)
with(glm_simple_roc, points(1 - FPR, TPR, col=1 + labels, cex = 0.7))

require(rpart)
require(rpart.plot)

###arbre_deci2
dt.don.groupe<-rpart(Pedagogie~., data=don.groupe)
prp(dt.don.groupe, extra = 1+100,type = 2, under=TRUE, yesno=2)
plotcp(dt.don.groupe)

dt.audela<-rpart(Pedagogie~audela, data=don.groupe)
prp(dt.audela, extra = 1+100,type = 2, under=TRUE, yesno=2)
plotcp(dt.audela)

datadt2<-dataPropre[,14:41]
dt2<-rpart(dataPropre$Pedagogie~ .,datadt2,control=rpart.control(minsplit=5,cp=0), method = 'class')
prp(dt2, extra = 1+100,type = 2, under=TRUE, yesno=2)
plotcp(dt2)

####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")), file = "export/arbre_deci.RData")


rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))
