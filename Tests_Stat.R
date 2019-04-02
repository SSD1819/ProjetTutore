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

dataPropreP1<-subset(dataPropre, Pedagogie == "Conventionnelle")
dataPropreP2<-subset(dataPropre, Pedagogie == "Montessori")


#### Tests ####

# T1 t-test

tt<-t.test(dataPropreP1$T1, dataPropreP2$T1, alternative = "two.sided")

# the rest - proportion tests

d=data.frame(x=rep(0,27))
for (i in 14:41){
  mat.i<-cbind(matrix(table(dataPropreP1[,i])), matrix(table(dataPropreP2[,i])))
  prop.i<-prop.test(mat.i, alternative = "two.sided", correct = FALSE)
  d[i,] = prop.i$p.value
}

d<-data.frame(d[-c(1:13),])
row.names(d)<-colnames(dataPropre[,14:41])
colnames(d)<-c("p-values of prop.test")
d[29,]=tt$p.value
rownames(d)[29]<-"T1"

### Final result of p-values (two.sided) ###

print(d)
subset(d, d[,1] < 0.05) #show the rows p-value is smaller than 0.05

#####################
### T51, T88, T89 ###
#####################

### Trying with less option ###

tt1<-t.test(dataPropreP1$T1, dataPropreP2$T1, alternative = "less")

d1=data.frame(x=rep(0,27))
for (i in 14:41){
  mat1.i<-cbind(matrix(table(dataPropreP1[,i])), matrix(table(dataPropreP2[,i])))
  prop1.i<-prop.test(mat1.i, alternative = "less", correct = FALSE)
  d1[i,] = prop1.i$p.value
}

d1<-data.frame(d1[-c(1:13),])
row.names(d1)<-colnames(dataPropre[,14:41])
colnames(d1)<-c("p-values of prop.test")
d1[29,]=tt1$p.value
rownames(d1)[29]<-"T1"

### Final result of p-values (less) ###

print(d1)
subset(d1, d1[,1] < 0.05) 

################
### T21, T51 ###
################

### Trying with greater option ###

tt2<-t.test(dataPropreP1$T1, dataPropreP2$T1, alternative = "greater")

d2=data.frame(x=rep(0,27))
for (i in 14:41){
  mat2.i<-cbind(matrix(table(dataPropreP1[,i])), matrix(table(dataPropreP2[,i])))
  prop2.i<-prop.test(mat2.i, alternative = "greater", correct = FALSE)
  d2[i,] = prop2.i$p.value
}

d2<-data.frame(d2[-c(1:13),])
row.names(d2)<-colnames(dataPropre[,14:41])
colnames(d2)<-c("p-values of prop.test")
d2[29,]=tt2$p.value
rownames(d2)[29]<-"T1"

### Final result of p-values (greater) ###

print(d2)
subset(d2, d2[,1] < 0.05) 

##########################
### T81, T87, T88, T89 ###
##########################


##############################################
### Testing with new variables (two.sided) ###

don.groupeP1<-subset(don.groupe, Pedagogie == "Conventionnelle")
don.groupeP2<-subset(don.groupe, Pedagogie == "Montessori")

m<-cbind(matrix(table(don.groupeP1[,2])), matrix(table(don.groupeP2[,2])))
pt<-prop.test(m, alternative = "two.sided", correct = FALSE)
df1 = pt$p.value
tt<-t.test(don.groupeP1$audela, don.groupeP2$audela)
df2  = tt$p.value
tt<-t.test(don.groupeP1$outils, don.groupeP2$outils)
df3 = tt$p.value
tt<-t.test(don.groupeP1$objet, don.groupeP2$objet)
df4  = tt$p.value

df=data.frame(x=rep(0,4))
df[1,]=df1
df[2,]=df2
df[3,]=df3
df[4,]=df4

colnames(df)<-c("p-values/prop.test - New Variables")
row.names(df)<-colnames(don.groupe[,2:5])
df

### Testing with greater ###

m<-cbind(matrix(table(don.groupeP1[,2])), matrix(table(don.groupeP2[,2])))
pt<-prop.test(m, alternative = "greater", correct = FALSE)
df1 = pt$p.value
tt<-t.test(don.groupeP1$audela, don.groupeP2$audela, alternative = "greater")
df2 = tt$p.value
tt<-t.test(don.groupeP1$outils, don.groupeP2$outils, alternative = "greater")
df3 = tt$p.value
tt<-t.test(don.groupeP1$objet, don.groupeP2$objet, alternative = "greater")
df4 = tt$p.value

df=data.frame(prop.i=rep(0,4))
df[1,]=df1
df[2,]=df2
df[3,]=df3
df[4,]=df4

colnames(df)<-c("p-values/prop.test - New Variables")
row.names(df)<-colnames(don.groupe[,2:5])
df


### Testing with less ###

m<-cbind(matrix(table(don.groupeP1[,2])), matrix(table(don.groupeP2[,2])))
pt<-prop.test(m, alternative = "less", correct = FALSE)
df1 = pt$p.value
tt<-t.test(don.groupeP1$audela, don.groupeP2$audela, alternative = "less")
df2  = tt$p.value
tt<-t.test(don.groupeP1$outils, don.groupeP2$outils, alternative = "less")
df3 = tt$p.value
tt<-t.test(don.groupeP1$objet, don.groupeP2$objet, alternative = "less")
df4  = tt$p.value

df=data.frame(prop.i=rep(0,4))
df[1,]=df1
df[2,]=df2
df[3,]=df3
df[4,]=df4

colnames(df)<-c("p-values/prop.test - New Variables")
row.names(df)<-colnames(don.groupe[,2:5])
df


# # Visualisation of significantly different data
# par(mfrow=c(2,3))
# # T72
# T72 <- rbind(table(dataPropreP1$T72),table(dataPropreP2$T72))
# barplot(T72, beside = T, col = c("blue", "green"), main="T72 : P1>P2",
#         legend.text =c("P1","P2"),args.legend = list(x = "topright"))
# 
# # T81
# T81 <- rbind(table(dataPropreP1$T81),table(dataPropreP2$T81))
# barplot(T81, beside = T, col = c("blue", "green"), main="T81 : P1<P2",
#         legend.text =c("P1","P2"),args.legend = list(x = "top"))
# 
# # T87
# T87 <- rbind(table(dataPropreP1$T87),table(dataPropreP2$T87))
# barplot(T87, beside = T, col = c("blue", "green"), main="T87 : P1<P2",
#         legend.text =c("P1","P2"),args.legend = list(x = "top"))
# 
# # T88
# T88 <- rbind(table(dataPropreP1$T88),table(dataPropreP2$T88))
# barplot(T88, beside = T, col = c("blue", "green"), main="T88 : P1<P2",
#         legend.text =c("P1","P2"),args.legend = list(x = "top"))
# 
# # T89
# T89 <- rbind(table(dataPropreP1$T89),table(dataPropreP2$T89))
# barplot(T89, beside = T, col = c("blue", "green"), main="T89 : P1<P2",
#         legend.text =c("P1","P2"),args.legend = list(x = "top"))
# 
# # audela
# tt<-table(don.groupeP2$audela)
# tt["5"]=0
# tt1=tt
# for (i in 6:12){
#   tt1[6]=tt[13]
#   tt1[i+1]=tt[i]
# }
# names(tt1)<-c(0:12)
# 
# audela <- rbind(table(don.groupeP1$audela),tt1)
# barplot(audela, beside = T, col = c("blue", "green"), main="audela : P1<P2",
#         legend.text =c("P1","P2"),args.legend = list(x = "topright"))


# Visualisation of significantly different data
par(mfrow=c(2,3))
# T21
T21 <- rbind(table(dataPropreP1$T21),table(dataPropreP2$T21))
barplot(T21, beside = T, col = c("blue", "green"), main="T21 : P1<P2")

# T51
T51 <- rbind(table(dataPropreP1$T51),table(dataPropreP2$T51))
barplot(T51, beside = T, col = c("blue", "green"), main="T51 : P1<P2")

# T81
T81 <- rbind(table(dataPropreP1$T81),table(dataPropreP2$T81))
barplot(T81, beside = T, col = c("blue", "green"), main="T81 : P1>P2")

# T87
T87 <- rbind(table(dataPropreP1$T87),table(dataPropreP2$T87))
barplot(T87, beside = T, col = c("blue", "green"), main="T87 : P1>P2")

par(xpd=TRUE)
legend(2.5,-15,c("P1", "P2"), fill =  c("blue", "green"), box.lwd = 1)

# T88
T88 <- rbind(table(dataPropreP1$T88),table(dataPropreP2$T88))
barplot(T88, beside = T, col = c("blue", "green"), main="T88 : P1>P2")

# T89
T89 <- rbind(table(dataPropreP1$T89),table(dataPropreP2$T89))
barplot(T89, beside = T, col = c("blue", "green"), main="T89 : P1>P2")

mtext("Visualisation de données significativement différentes", 
      side = 3, line = -16.5, outer = TRUE)


#save the data for report
save(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")),
     file = "export/Tests_Stat.RData")

# keeping initial environment
par(mfrow=c(1,1))
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec", "don.groupe", "dataSumOld", "dataVecOld")))

