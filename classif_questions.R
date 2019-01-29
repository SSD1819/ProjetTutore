####Préparation jeu de données pour les questions####

#On enlève tout ce qui n'a pas de rapport avec les questions
dataQuestions<-dataPropre[,13:46]

#On enlève les question T11 qui ne servent à rien
dataQuestions<-dataQuestions[,-c(32:34)]

#On enlève les sous-questions (sauf pour la Q4)
dataQuestions<-dataQuestions[,-c(3,4,6,16,18,20,22:29,31)]

#Extraction de la question 4
Q4<-dataQuestions[,4:11]
Q4[is.na(Q4)]<-"2"

#Mise en forme de la question 4 en une seule variable.
#Le code de cette variable est sous la forme 41a41b41c41d42a42b42c42d
#Où :
#0 signifie que l'enfant a mal répondu
#1 signifie que l'enfant a bien répondu
#2 signifie que la question n'a pas été posée a l'enfant
T4.Total<-paste(Q4$T41a.TOTAL,Q4$T41b.TOTAL,Q4$T41cTOTAL,Q4$T41d.TOTAL,Q4$T42a.TOTAL,Q4$T42b.TOTAL,Q4$T42c.TOTAL,Q4$T42d.TOTAL)

#Remplacement de la Q4 par le code dans la dataFrame
dataQuestions[,4]<-T4.Total
dataQuestions<-dataQuestions[,-c(5:11)]
rm(Q4,T4.Total)

#Tranformations en quali
Quali<-function(x){
  x<- as.character(x)
}
dataQuestions<-as.data.frame(apply(X=dataQuestions,MARGIN=2,FUN=Quali))
dataQuestions$T1.Réponse<-as.numeric(dataQuestions$T1.Réponse)
####Classification (T1 Quali)####
require(ClustOfVar)

###CAH
test<-hclustvar(X.quanti=dataQuestions[,1],X.quali=dataQuestions[,-1])
plot(test)
rect.hclust(test,6)

###Kmeans (marche pas encore)
testk<-kmeansvar(X.quanti=dataQuestions[,1],X.quali=dataQuestions[,-1],rename.level=TRUE)
