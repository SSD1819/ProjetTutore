summary(dataPropre)

nom<-NULL
for (i in 1:ncol(dataPropre)){nom<-paste0(nom,",",colnames(dataPropre)[i])}
nom
quest<-c("T21.TOTAL","T22.TOTAL","T23.TOTAL","T31TOTAL",
         "T32.TOTAL","T41a.TOTAL","T41b.TOTAL","T41cTOTAL",
         "T41d.TOTAL","T42a.TOTAL","T42b.TOTAL","T42c.TOTAL",
         "T42d.TOTAL","T51.TOTAL","T52.TOTAL","T61.TOTAL",
         "T62TOTAL","T71.TOTAL","T72TOTAL","T81.TOTAL","T82.TOTAL",
         "T83.TOTAL","T84.TOTAL","T85.TOTAL","T86.TOTAL",
         "T87.TOTAL","T88.TOTAL","T89.TOTAL","T91.Total",
         "T92.Total","T111.TOTAL","T112.TOTAL","T113.TOTAL")
don.na<-NULL
don.na<-dataPropre[,quest]
don.na<-don.na[,colSums(sapply(lapply(don.na,is.na),as.numeric))==0] #Suppression des questions avec NA
#à refaire donc avec les scores
rownames(don.na)<-paste0(1:nrow(dataPropre),"_",dataPropre$T1.Réponse)
