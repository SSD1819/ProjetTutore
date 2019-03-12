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

### Final result of p-values ###

print(d)
subset(d, d[,1] < 0.05) #show the rows p-value is smaller than 0.05

### Trying with less option ###

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

### Final result of p-values ###

print(d)
subset(d, d[,1] < 0.05) 

### NO NEW RESULTS ###

### Trying with greater option ###

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

### Final result of p-values ###

print(d)
subset(d, d[,1] < 0.05) 

### Here we have ONLY T81 added, see the visualisation after ###

# Visualisation of significat difference data

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

# T87
T81 <- rbind(prop.table(table(dataPropreP1$T81)),prop.table(table(dataPropreP2$T81)))
barplot(T81, beside = T, col = c("blue", "green"), main="T81 reponses (greater)",
        legend.text =c("P1","P2"),args.legend = list(x = "top"))


# keeping initial environment
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")))
