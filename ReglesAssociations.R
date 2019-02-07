######Mise en forme d'un jeu de donnees######
######pour pouvoir faire des règles d'associations (que des 0 et des 1)

#### Test avec arules ####
require(arules)

#On met en forme P1 et P2
p1<-rep(0,length(dataPropre$Pedagogie))
p2<-rep(0,length(dataPropre$Pedagogie))
p1[which(dataPropre$Pedagogie=="P1")]<-"1"
p2[which(dataPropre$Pedagogie=="P2")]<-"1"

#On ne récupère que les questions (sauf la 1)
#On ajoute p1 et p2
datAssos<-cbind(p1,p2,dataPropre[,14:41])

#Transformation de datassos en tableau de variables numériques
datAssos<-apply(datAssos,2,as.numeric)

#### Règles d'associations avec fonction apriori() ####
rules <- apriori(datAssos, parameter = list(supp = 0.7, conf = 0.9, minlen = 2, maxlen = 10, target = "rules"))
summary(rules)
rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift,200))

#### Règles d'associations sans la question 8 ####
rulesWO8 <- apriori(datAssos[,-c(22:30)], parameter = list(supp = 0.5, conf = 0.9, minlen = 2, maxlen = 10, target = "rules"))
summary(rulesWO8)
rules_lift <- sort (rulesWO8, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift,200))

#### Test avec la méthode du TP3 Applied proba ####

#1-item support
support1<-apply(X=datAssos,MARGIN=2,FUN=mean)
support1

#2-item support
support2<-t(datAssos)%*%datAssos
support2<-support2/length(datAssos[,1])
support2

#Confidence
confidence<-support2/support1
confidence

#lift
lift<-support2/(support1%*%t(support1))
lift

###Règles
suppMin<-0.5
confMin<-0.5
liftMin<-1.1

#Matrice des règles d'associations
reglesMat<-(support2>=suppMin)*(confidence>=confMin)*(lift>=liftMin)

#Suppression des associations inutiles du type A=>A
diag(reglesMat)<-0

#Règles écrites (et non en matrice)
posSignif<-which(reglesMat==1)
iSignif<-posSignif%%length(reglesMat[,1])
jSignif<-ceiling(posSignif/length(reglesMat[,1]))
regles<-paste(rownames(reglesMat[iSignif,jSignif]),"=>",colnames(reglesMat[iSignif,jSignif]),sep=" ")
regles

rm(datAssos,p1,p2,support1,support2,confidence,lift,suppMin,confMin,liftMin,reglesMat,posSignif,iSignif,jSignif,regles, rules, rules_lift, rulesWO8)
