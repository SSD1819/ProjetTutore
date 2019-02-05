######Mise en forme d'un jeu de donnees#####
######pour pouvoir faire des règles d'associations (que des 0 et des 1)

require(arules)

#On met en forme P1 et P2
p1<-rep(0,length(dataPropre$Pedagogie))
p2<-rep(0,length(dataPropre$Pedagogie))
p1[which(dataPropre$Pedagogie=="P1")]<-"1"
p2[which(dataPropre$Pedagogie=="P2")]<-"1"

#On ne récupère que les questions (sauf la 1)
#On ajoute p1 et p2
datAssos<-cbind(p1,p2,dataPropre[,14:41])

apriori(datAssos)

rm(datAssos,p1,p2)