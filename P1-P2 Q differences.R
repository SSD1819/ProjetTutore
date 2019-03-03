# P1-P2 Questions differences: tests and visualisation

# Creation of dataframes containing only respectively P1 and P2 

dataPropreP1<-subset(dataPropre, Pedagogie == "P1")
dataPropreP2<-subset(dataPropre, Pedagogie == "P2")


#### for each question ####

# T1
boxplot(dataPropre$T1~dataPropre$Pedagogie,main="T1 Reponses (P1)")
t.test(dataPropreP1$T1, dataPropreP2$T1)

# T21
T21 <- rbind(prop.table(table(dataPropreP1$T21)),prop.table(table(dataPropreP2$T21)))
barplot(T21, beside = T, col = c("blue", "green"), main="T21 reponses",
        legend.text =c("P1","P2"),args.legend = list(x = "topleft"))

chi2<-chisq.test(table(dataPropreP1$T21),table(dataPropreP2$T21))
chi2


# T22
T22 <- rbind(prop.table(table(dataPropreP1$T22)),prop.table(table(dataPropreP2$T22)))
barplot(T22, beside = T, col = c("blue", "green"), main="T22 reponses",
        legend.text =c("P1","P2"),args.legend = list(x = "top"))

chi2<-chisq.test(table(dataPropreP1$T22),table(dataPropreP2$T22))
chi2


# T23
T23 <- rbind(prop.table(table(dataPropreP1$T23)),prop.table(table(dataPropreP2$T23)))
barplot(T23, beside = T, col = c("blue", "green"), main="T23 reponses",
        legend.text =c("P1","P2"),args.legend = list(x = "topright"))

chi2<-chisq.test(table(dataPropreP1$T23),table(dataPropreP2$T23))
chi2

# T31
T31 <- rbind(prop.table(table(dataPropreP1$T31)),prop.table(table(dataPropreP2$T31)))
barplot(T31, beside = T, col = c("blue", "green"), main="T31 reponses",
        legend.text =c("P1","P2"),args.legend = list(x = "topleft"))

chi2<-chisq.test(table(dataPropreP1$T31),table(dataPropreP2$T31))
chi2

# T32
T32 <- rbind(prop.table(table(dataPropreP1$T32)),prop.table(table(dataPropreP2$T32)))
barplot(T32, beside = T, col = c("blue", "green"), main="T32 reponses",
        legend.text =c("P1","P2"),args.legend = list(x = "topright"))

chi2<-chisq.test(table(dataPropreP1$T32),table(dataPropreP2$T32))
chi2

# T51
T51 <- rbind(prop.table(table(dataPropreP1$T51)),prop.table(table(dataPropreP2$T51)))
barplot(T51, beside = T, col = c("blue", "green"), main="T51 reponses",
        legend.text =c("P1","P2"),args.legend = list(x = "topright"))

chi2<-chisq.test(table(dataPropreP1$T51),table(dataPropreP2$T51))
chi2

# T52
T52 <- rbind(prop.table(table(dataPropreP1$T52)),prop.table(table(dataPropreP2$T52)))
barplot(T52, beside = T, col = c("blue", "green"), main="T52 reponses",
        legend.text =c("P1","P2"),args.legend = list(x = "topright"))

chi2<-chisq.test(table(dataPropreP1$T52),table(dataPropreP2$T52))
chi2



#### testing prop.test ####

# T21
prop.test(table(dataPropreP1$T21), alternative = "two.sided")
prop.test(table(dataPropreP2$T21), alternative = "two.sided")

# T22
prop.test(table(dataPropreP1$T22), alternative = "two.sided")
prop.test(table(dataPropreP2$T22), alternative = "two.sided")

# T23
prop.test(table(dataPropreP1$T23), alternative = "two.sided")
prop.test(table(dataPropreP2$T23), alternative = "two.sided")

# T31
prop.test(table(dataPropreP1$T31), alternative = "two.sided")
prop.test(table(dataPropreP2$T31), alternative = "two.sided")

# T32
prop.test(table(dataPropreP1$T32), alternative = "two.sided")
prop.test(table(dataPropreP2$T32), alternative = "two.sided")

# T51
prop.test(table(dataPropreP1$T51), alternative = "two.sided")
prop.test(table(dataPropreP2$T51), alternative = "two.sided")

# T52
prop.test(table(dataPropreP1$T52), alternative = "two.sided")
prop.test(table(dataPropreP2$T52), alternative = "two.sided")



# keeping initial environment
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")))
