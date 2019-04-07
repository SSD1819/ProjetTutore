##Graphe intéressant près analyse



nom<-c("T21","T22","T23","T31",
       "T32","T41a","T41b","T41c","T41d","T42a",
       "T42b","T42c","T42d","T51","T52","T61",
       "T62","T71","T72","T81","T82","T83",
       "T84","T85","T86","T87","T88","T89")
df<-dataPropre[,nom]

scoretotal<-rowSums(apply(df,2,as.numeric))
ttm<-by(scoretotal[dataPropre$Pedagogie=="Montessori"],
        dataPropre$AnneeScolaire[dataPropre$Pedagogie=="Montessori"],mean)
ttc<-by(scoretotal[dataPropre$Pedagogie=="Conventionnelle"],
        dataPropre$AnneeScolaire[dataPropre$Pedagogie=="Conventionnelle"],mean)
plot(scoretotal~dataPropre$Pedagogie)
plot(scoretotal~dataPropre$AnneeScolaire)
plot(ttc,type="l",ylim = c(10,20),col="red")
lines(ttm,col="blue")

df<-data.frame(score=scoretotal,Pedagogie=dataPropre$Pedagogie,Annee=dataPropre$AnneeScolaire)

noms<-c("Pedagogie", "T1","T2","T3",
        "T5","T61","T62","T71","T72",
        "T8.123","T8.456789")
score<-dataSum[noms]
boxplot(score[,-c(1,2)])

###changement de la df pour ggplot2
# install.packages("reshape")
library(reshape)
md.df <- melt(score, id=(c("Pedagogie", "T1")))

ggplot(md.df,aes(variable,value)) + 
  geom_boxplot()



####Exportation des data pour l'app Shiny####
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")), file = "export/graphexplo.RData")

####Suppression de ce qui ne nous sert plus####
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe")))



