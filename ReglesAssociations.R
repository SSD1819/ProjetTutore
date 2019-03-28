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

rulesP1 <- apriori(datAssos[which(p1==1 & datAssos[,"T51"]==1),-c(1,2,22:30)], parameter = list(supp = 0.5, conf = 0.9, minlen = 2, maxlen = 10, target = "rules"))
summary(rulesP1)
rules_liftP1 <- sort (rulesP1, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_liftP1,200))

rulesP2 <- apriori(datAssos[which(p1==0 & datAssos[,"T51"]==1),-c(1,2,22:30)], parameter = list(supp = 0.5, conf = 0.9, minlen = 2, maxlen = 10, target = "rules"))
summary(rulesP2)
rules_liftP2 <- sort (rulesP2, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_liftP2,200))

####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")), file = "export/ReglesAssociations.RData")

####Suppression de ce qui ne nous sert plus####
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))
