require(rpart)
require(rpart.plot)
set.seed(1)

########Arbre et prediction pour les variables de don.groupe
data1 = sort(sample(nrow(don.groupe), nrow(don.groupe)*.7))
don.groupe.train<-don.groupe[data1,]
don.groupe.test<-don.groupe[-data1,]

param1<-rpart.control(minsplit = 30, minbucket = 10)
dt1<-rpart(Pedagogie~audela+outils+objet, data=don.groupe.train, control = param1)
prp(dt1, extra = 1+100,type = 2, under=TRUE, yesno=2)
plotcp(dt1)

pred.pedagogie1<-predict(dt1, newdata=don.groupe.test, type="class")
print(summary(pred.pedagogie))
result1 <- data.frame(Pedagogie.pred=pred.pedagogie, Pedagogie.real=don.groupe.test["Pedagogie"])
result.pred1<-mean(as.numeric(result1[,1]==result1[,2]))



######Arbre et prediction pour les variables de dataPropre
dataArbre2<-dataPropre[,c(2,13:41)]
data2 = sort(sample(nrow(dataArbre2), nrow(dataArbre2)*.7))
dataArbre2.train<-dataArbre2[data2,]
dataArbre2.test<-dataArbre2[-data2,]

param2<-rpart.control(minsplit = 30, minbucket = 10)
dt2<-rpart(Pedagogie~., data=dataArbre2.train, control = param2)
prp(dt2, extra = 1+100,type = 2, under=TRUE, yesno=2)
plotcp(dt2)

pred.pedagogie2<-predict(dt2, newdata=dataArbre2.test, type="class")
print(summary(pred.pedagogie2))
result2 <- data.frame(Pedagogie.pred=pred.pedagogie2, Pedagogie.real=dataArbre2.test["Pedagogie"])
result.pred2<-mean(as.numeric(result2[,1]==result2[,2]))


######Arbre et prediction pour les variables de dataPropre
dataArbre3<-dataPropre[,c("Pedagogie","T21" , "T22" , "T31" , "T41b", "T51", "T81", "T83",  "T89")]
data3 = sort(sample(nrow(dataArbre3), nrow(dataArbre3)*.7))
dataArbre3.train<-dataArbre3[data3,]
dataArbre3.test<-dataArbre3[-data3,]

param3<-rpart.control(minsplit = 30, minbucket = 10)
dt3<-rpart(Pedagogie~., data=dataArbre3.train, control = param3)
prp(dt3, extra = 1+100,type = 2, under=TRUE, yesno=2)
plotcp(dt3)

pred.pedagogie3<-predict(dt3, newdata=dataArbre3.test, type="class")
print(summary(pred.pedagogie3))
result3 <- data.frame(Pedagogie.pred=pred.pedagogie3, Pedagogie.real=dataArbre3.test["Pedagogie"])
result.pred3<-mean(as.numeric(result3[,1]==result3[,2]))


#######Arbre et prediction pour les variables de dataVec
set.seed(2)
dataArbre4<-dataVecOld[,c(2,14:32)]
data4 = sort(sample(nrow(dataArbre4), nrow(dataArbre4)*.7))
dataArbre4.train<-dataArbre4[data4,]
dataArbre4.test<-dataArbre4[-data4,]

mean(as.numeric(dataArbre.train[,1]==dataArbre4.train[,1]))

param4<-rpart.control(minsplit = 30, minbucket = 10)
dt4<-rpart(Pedagogie~., data=dataArbre4.train, control = param4)
prp(dt4, extra = 1+100,type = 2, under=TRUE, yesno=2)
plotcp(dt4)

pred.pedagogie4<-predict(dt4, newdata=dataArbre4.test, type="class")
print(summary(pred.pedagogie4))
result4 <- data.frame(Pedagogie.pred=pred.pedagogie4, Pedagogie.real=dataArbre4.test["Pedagogie"])
result.pred4<-mean(as.numeric(result4[,1]==result4[,2]))




print(result.pred1)
print(result.pred2)
print(result.pred3)
print(result.pred4)