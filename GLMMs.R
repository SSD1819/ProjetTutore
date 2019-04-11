# Names of necassary columns

quest<-c("AgeInt","T1", "T21","T22","T23","T31","T32",
         "T41c","T41b","T41a","T41d","T42a","T42b","T42c","T42d",
         "T51","T52","T61","T62","T71","T72" ,             
         "T81","T82","T83","T84",
         "T85","T86","T87","T88",
         "T89","AnneeScolaire","newClasse")

# extraction from initial dataframe

don.reg<-dataPropre[,c("Pedagogie",quest)]

# glmm
if (!require(glmm)) install.packages("glmm")

# partie gauche glmm demand numeric 0 et 1
for (i in 3:(ncol(don.reg)-2)){
  don.reg[,i]<-as.numeric(as.character(don.reg[,i]))
}


##### glmm for each of the questions #####

#T21
T21mm<-glmm(T21 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T21mm)

#T22
T22mm<-glmm(T22 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T22mm)

#T23
T23mm<-glmm(T23 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T23mm)

#T31
T31mm<-glmm(T31 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T31mm)

#T32
T32mm<-glmm(T32 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T32mm)

#T41a
T41amm<-glmm(T41a ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T41amm)

#T41b
T41bmm<-glmm(T41b ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T41bmm)

#T41c
T41cmm<-glmm(T41c ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T41cmm)

#T41d
T41dmm<-glmm(T41d ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T41dmm)

#T42a
T42amm<-glmm(T42a ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T42amm)

#T42b
T42bmm<-glmm(T42b ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T42bmm)

#T42c
T42cmm<-glmm(T42c ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T42cmm)

#T42d
T42dmm<-glmm(T42d ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T42dmm)

#T51
T51mm<-glmm(T51 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T51mm)

#T52
T52mm<-glmm(T52 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T52mm)

#T61
T61mm<-glmm(T61 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T61mm)

#T62
T62mm<-glmm(T62 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T62mm)

#T71
T71mm<-glmm(T71 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T71mm)

#T72
T72mm<-glmm(T72 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T72mm)

#T81
T81mm<-glmm(T81 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T81mm)

#T82
T82mm<-glmm(T82 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T82mm)

#T83
T83mm<-glmm(T83 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T83mm)

#T84
T84mm<-glmm(T84 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T84mm)

#T85
T85mm<-glmm(T85 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T85mm)

#T86
T86mm<-glmm(T86 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T86mm)

#T87
T87mm<-glmm(T87 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T87mm)

#T88
T88mm<-glmm(T88 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)


summary(T88mm)

#T89
T89mm<-glmm(T89 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^5, debug=TRUE)

summary(T89mm)

#the first column reports the number of succeses and the second reports the number of failures



# Save the data for report
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")),
     file = "export/glmm_vs_taches.RData")

# Supression des variables qui ne servent à rien
# rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))

