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

require(lme4)
zz<-glmer(T21 ~ Pedagogie + AgeInt + AnneeScolaire + (1|newClasse),
      family = binomial, data=don.reg)
summary(zz)
 
##### glmm for each of the questions #####

#T21
T21mm<-glmm(T21 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T21mm)
 
## T21 - On supprime Age
T21mm.Ped.An<-glmm(T21 ~ Pedagogie + AnneeScolaire,
              random = list(~ 0 + newClasse),
              varcomps.names=c("newClasse"),data=don.reg,
              family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T21mm.Ped.An)

## T21 - On supprime Annee aussi
T21mm.Ped<-glmm(T21 ~ Pedagogie,
                   random = list(~ 0 + newClasse),
                   varcomps.names=c("newClasse"),data=don.reg,
                   family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T21mm.Ped)

#### Aucune variable n'est significative. ####

#T22
T22mm<-glmm(T22 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T22mm)

##T22 - On supprime annee
T22mm.Ped.Age<-glmm(T22 ~ Pedagogie + AgeInt ,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T22mm.Ped.Age)

##T22 - On supprime aussi Pedagogie
T22mm.Age<-glmm(T22 ~ AgeInt ,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T22mm.Age)

#### Aucune variable n'est significative. ####

#T23
T23mm<-glmm(T23 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T23mm)

#T23 - On supprime Pedagogie
T23mm.Age.An<-glmm(T23 ~ AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T23mm.Age.An)

#T23 - On supprime aussi Annee
T23mm.Age<-glmm(T23 ~ AgeInt,
                   random = list(~ 0 + newClasse),
                   varcomps.names=c("newClasse"),data=don.reg,
                   family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T23mm.Age)

#T23 - On supprime aussi Age
T23mm.An<-glmm(T23 ~ AnneeScolaire,
                random = list(~ 0 + newClasse),
                varcomps.names=c("newClasse"),data=don.reg,
                family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T23mm.An)

#### Aucune variable n'est significative. ####

#T31
T31mm<-glmm(T31 ~ Pedagogie + AgeInt + AnneeScolaire ,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T31mm)

#T31 - On supprime Annee
T31mm.Ped.Age<-glmm(T31 ~ Pedagogie + AgeInt ,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T31mm.Ped.Age)

#T31 - On supprime aussi Age
T31mm.Ped<-glmm(T31 ~ Pedagogie  ,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T31mm.Ped)

#### Aucune variable n'est significative. ####

#T32
T32mm<-glmm(T32 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T32mm)

#T32 - On supprime Annee
T32mm.Ped.Age<-glmm(T32 ~ Pedagogie + AgeInt ,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T32mm.Ped.Age)

#T32 - On supprime aussi Age
T32mm.Ped<-glmm(T32 ~ Pedagogie ,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T32mm.Ped)

#### Aucune variable n'est significative. ####

#T41a
T41amm<-glmm(T41a ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41amm)

#T41a - On supprime Age
T41amm.Ped.An<-glmm(T41a ~ Pedagogie + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41amm.Ped.An)

#T41a - Trying only with Annee
T41amm.An<-glmm(T41a ~ AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41amm.An)

#T41a - Trying only with Pedagogie
T41amm.Ped<-glmm(T41a ~ Pedagogie,
                random = list(~ 0 + newClasse),
                varcomps.names=c("newClasse"),data=don.reg,
                family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41amm.Ped)

#### in T41a glmm with ONLY Annee there is 0.0695 p-value

#T41b
T41bmm<-glmm(T41b ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41bmm)

#T41b - on supprime Age
T41bmm.Ped.An<-glmm(T41b ~ Pedagogie + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41bmm.Ped.An)

#T41b - on supprime aussi Annee
T41bmm.Ped<-glmm(T41b ~ Pedagogie,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41bmm.Ped)

#T41b - on essaie supprimer Pedagogie
T41bmm.An<-glmm(T41b ~ AnneeScolaire,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41bmm.An)

#### Aucune variable n'est significative. ####

#T41c
T41cmm<-glmm(T41c ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41cmm)

#T41c - On supprime Age
T41cmm.Ped.An<-glmm(T41c ~ Pedagogie + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41cmm.Ped.An)

#T41c - On supprime aussi Pedagogie
T41cmm.An<-glmm(T41c ~ AnneeScolaire,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41cmm.An)

#### Annee est significatif pour T41c

#T41d
T41dmm<-glmm(T41d ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41dmm)

#T41d - On supprime Age
T41dmm.Ped.An<-glmm(T41d ~ Pedagogie + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41dmm.Ped.An)

#T41d - On supprime aussi Pedagogie
T41dmm.An<-glmm(T41d ~  AnneeScolaire,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T41dmm.An)

#### Annee est significatif pour T41d

#T42a
T42amm<-glmm(T42a ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42amm)

#T42a On supprime Age
T42amm.Ped.An<-glmm(T42a ~ Pedagogie + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42amm.Ped.An)

#T42a On supprime Age, apres on essaie de supprimer Annee aussi
T42amm.Ped<-glmm(T42a ~ Pedagogie,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42amm.Ped)

#T42a On supprime Age, apres on essaie de supprimer Pedagogie aussi
T42amm.An<-glmm(T42a ~ AnneeScolaire,
                 random = list(~ 0 + newClasse),
                 varcomps.names=c("newClasse"),data=don.reg,
                 family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42amm.An)

#### Aucune variable n'est significative. ####

#T42b
T42bmm<-glmm(T42b ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42bmm)

#T42b - On supprime Pedagogie
T42bmm.Age.An<-glmm(T42b ~ AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42bmm.Age.An)

#T42b - On supprime Pedagogie, apres on supprime Annee aussi
T42bmm.Age<-glmm(T42b ~ AgeInt,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42bmm.Age)

#T42b - On supprime Pedagogie, apres on essaie supprimer Age aussi
T42bmm.An<-glmm(T42b ~ AnneeScolaire,
                 random = list(~ 0 + newClasse),
                 varcomps.names=c("newClasse"),data=don.reg,
                 family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42bmm.An)

#### Aucune variable n'est significative. ####

#T42c 
T42cmm<-glmm(T42c ~ Pedagogie + AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42cmm)

#T42c - On supprime Pedagogie
T42cmm.Age.An<-glmm(T42c ~ AgeInt + AnneeScolaire,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42cmm.Age.An)

#T42c - On supprime Pedagogie, apres on supprime aussi Annee
T42cmm.Age<-glmm(T42c ~ AgeInt,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42cmm.Age)

#T42c - On supprime Pedagogie, apres on essaie supprimer Age aussi
T42cmm.An<-glmm(T42c ~  AnneeScolaire,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42cmm.An)

#### Aucune variable n'est significative. ####

#T42d 
T42dmm<-glmm(T42d ~ Pedagogie + AgeInt + AnneeScolaire ,
             random = list(~ 0 + newClasse),
             varcomps.names=c("newClasse"),data=don.reg,
             family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42dmm)

#T42d - On supprime Age
T42dmm.Ped.An<-glmm(T42d ~ Pedagogie + AnneeScolaire ,
               random = list(~ 0 + newClasse),
               varcomps.names=c("newClasse"),data=don.reg,
               family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42dmm.Ped.An)

#T42d - On supprime Age, apres on supprime Pedagogie aussi
T42dmm.An<-glmm(T42d ~ AnneeScolaire ,
                    random = list(~ 0 + newClasse),
                    varcomps.names=c("newClasse"),data=don.reg,
                    family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T42dmm.An)

#### in T42a glmm Annee with one modalite est significatif

#T51
T51mm<-glmm(T51 ~ Pedagogie + AgeInt + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T51mm)

#T51 - On supprime Age
T51mm.Ped.An<-glmm(T51 ~ Pedagogie + AnneeScolaire,
            random = list(~ 0 + newClasse),
            varcomps.names=c("newClasse"),data=don.reg,
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T51mm.Ped.An)

#T51 - On supprime Age, et on supprime Annee aussi
T51mm.Ped<-glmm(T51 ~ Pedagogie,
                   random = list(~ 0 + newClasse),
                   varcomps.names=c("newClasse"),data=don.reg,
                   family.glmm = binomial.glmm, m=10^4, debug=TRUE)

summary(T51mm.Ped)

# Pour T51 Pedagogie est est tres proche pour etre significatif 

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
            family.glmm = binomial.glmm, m=10^4, debug=TRUE)

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
T81mm<-glmm(T81 ~ Pedagogie,
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
     file = "export/glmm_1partie.RData")

# Supression des variables qui ne servent à rien
# rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))

