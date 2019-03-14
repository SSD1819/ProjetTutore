# P1-P2 Questions differences: tests and visualisation

### Information about correct statement in test functions ###
## chi-square test##

# The 'correct=FALSE' option in the chisq.test function turns off Yates' correction for
# the chi-square test (which is used with small sample sizes), and gives the standard chi-square test 
# statistic. R gives a two-tailed p-value. Note that the title for the output,
# 'Pearson's Chi-squared test' indicates that these results are for the uncorrected (not Yates'
# adjusted) chi-square test.

## prop.test ##

# To use the usual large-sample formula in calculating the confidence interval,
# include the 'correct=FALSE' option to turn off the small sample size correction factor 
# in the calculation (although in this example, with only 17 subjects in the control group, 
# the small sample version of the confidence interval might be more appropriate).


### chi-square test example ###
# T21 <- rbind(prop.table(table(dataPropreP1$T21)),prop.table(table(dataPropreP2$T21)))
# barplot(T21, beside = T, col = c("blue", "green"), main="T21 reponses",
#         legend.text =c("P1","P2"),args.legend = list(x = "topleft"))
# 
# chi2<-chisq.test(table(dataPropreP1$T21),table(dataPropreP2$T21),correct = FALSE)
# chi2
                                                                                                                                                                                       
# Creation of dataframes containing only respectively P1 and P2 

dataPropreP1<-subset(dataPropre, Pedagogie == "P1")
dataPropreP2<-subset(dataPropre, Pedagogie == "P2")


#### Tests ####

# T1 t-test

boxplot(dataPropre$T1~dataPropre$Pedagogie,main="T1 Reponses (P1)")
t.test(dataPropreP1$T1, dataPropreP2$T1)

# the rest - proportion tests

d=data.frame(prop.i=rep(0,27))
for (i in 14:41){
  mat.i<-cbind(matrix(table(dataPropreP1[,i])), matrix(table(dataPropreP2[,i])))
  prop.i<-prop.test(mat.i, alternative = "two.sided", correct = FALSE)
  d[i,] = prop.i$p.value
}

d<-data.frame(d[-c(1:13),])
row.names(d)<-colnames(dataPropre[,14:41])
colnames(d)<-c("p-values of prop.test")
d[29,]=t.test(dataPropreP1$T1, dataPropreP2$T1)$p.value
rownames(d)[29]<-"T1"

### Final result of p-values (two.sided) ###

print(d)
subset(d, d[,1] < 0.05) #show the rows p-value is smaller than 0.05

### Trying with less option ###

d=data.frame(prop.i=rep(0,27))
for (i in 14:41){
  mat.i<-cbind(matrix(table(dataPropreP1[,i])), matrix(table(dataPropreP2[,i])))
  prop.i<-prop.test(mat.i, alternative = "less", correct = FALSE)
  d[i,] = prop.i$p.value
}

d<-data.frame(d[-c(1:13),])
row.names(d)<-colnames(dataPropre[,14:41])
colnames(d)<-c("p-values of prop.test")
d[29,]=t.test(dataPropreP1$T1, dataPropreP2$T1)$p.value
rownames(d)[29]<-"T1"

### Final result of p-values (less) ###

print(d)
subset(d, d[,1] < 0.05) 

### NO NEW RESULTS ###

### Trying with greater option ###

d=data.frame(prop.i=rep(0,27))
for (i in 14:41){
  mat.i<-cbind(matrix(table(dataPropreP1[,i])), matrix(table(dataPropreP2[,i])))
  prop.i<-prop.test(mat.i, alternative = "greater", correct = FALSE)
  d[i,] = prop.i$p.value
}

d<-data.frame(d[-c(1:13),])
row.names(d)<-colnames(dataPropre[,14:41])
colnames(d)<-c("p-values of prop.test")
d[29,]=t.test(dataPropreP1$T1, dataPropreP2$T1)$p.value
rownames(d)[29]<-"T1"

### Final result of p-values (greater) ###

print(d)
subset(d, d[,1] < 0.05) 

### Here we have ONLY T81 added ###

# Visualisation of significantly different data

# T72
T72 <- rbind(prop.table(table(dataPropreP1$T72)),prop.table(table(dataPropreP2$T72)))
barplot(T72, beside = T, col = c("blue", "green"), main="T72 reponses (two.sided)",
        legend.text =c("P1","P2"),args.legend = list(x = "topright"))

# T87
T87 <- rbind(prop.table(table(dataPropreP1$T87)),prop.table(table(dataPropreP2$T87)))
barplot(T87, beside = T, col = c("blue", "green"), main="T87 reponses (two.sided)",
        legend.text =c("P1","P2"),args.legend = list(x = "top"))

# T88
T88 <- rbind(prop.table(table(dataPropreP1$T88)),prop.table(table(dataPropreP2$T88)))
barplot(T88, beside = T, col = c("blue", "green"), main="T88 reponses (two.sided)",
        legend.text =c("P1","P2"),args.legend = list(x = "top"))

# T89
T89 <- rbind(prop.table(table(dataPropreP1$T89)),prop.table(table(dataPropreP2$T89)))
barplot(T89, beside = T, col = c("blue", "green"), main="T89 reponses (two.sided)",
        legend.text =c("P1","P2"),args.legend = list(x = "top"))

# T81
T81 <- rbind(prop.table(table(dataPropreP1$T81)),prop.table(table(dataPropreP2$T81)))
barplot(T81, beside = T, col = c("blue", "green"), main="T81 reponses (greater)",
        legend.text =c("P1","P2"),args.legend = list(x = "top"))



##############################################
### Testing with new variables (two.sided) ###

don.groupeP1<-subset(don.groupe, Pedagogie == "P1")
don.groupeP2<-subset(don.groupe, Pedagogie == "P2")


# Class T1

mmm<-cbind(matrix(table(don.groupeP1[,2])), matrix(table(don.groupeP2[,2])))
ppp<-prop.test(mmm, alternative = "two.sided", correct = FALSE)
d2 = ppp$p.value

# audela

tempP2<-table(don.groupeP2[,3])
tempP2["5"]=0
tempP2<-matrix(tempP2[order(names(tempP2))])
tempP1<-matrix(table(don.groupeP1[,3])[order(names(table(don.groupeP1[,3])))])

mmm<-cbind(tempP1, tempP2)
ppp<-prop.test(mmm, alternative = "two.sided", correct = FALSE)
d3 = ppp$p.value

# outils

tempP2<-table(don.groupeP2[,4])
tempP2["0"]=0
tempP2<-matrix(tempP2[order(names(tempP2))])

mmm<-cbind(matrix(table(don.groupeP1[,4])), tempP2)
ppp<-prop.test(mmm, alternative = "two.sided", correct = FALSE)
d4 = ppp$p.value

# objet

mmm<-cbind(matrix(table(don.groupeP1[,5])), matrix(table(don.groupeP2[,5])))
ppp<-prop.test(mmm, alternative = "two.sided", correct = FALSE)
d5 = ppp$p.value

cc=data.frame(prop.i=rep(0,4))
cc[1,]=d2
cc[2,]=d3
cc[3,]=d4
cc[4,]=d5

colnames(cc)<-c("p-values/prop.test - New Variables")
row.names(cc)<-colnames(don.groupe[,2:5])
cc

### Even with alternative = "greater" and "less" we have the same results.
### NO Significant Difference whithin new variables



# keeping initial environment
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe")))
