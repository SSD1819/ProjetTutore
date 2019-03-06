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

chi2<-chisq.test(table(dataPropreP1$T22),table(dataPropreP2$T22),correct = FALSE)
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
mat<-cbind(matrix(table(dataPropreP1$T21)), matrix(table(dataPropreP2$T21)))
prop.test(mat, alternative = "less", correct = FALSE)
prop.test(mat, alternative = "greater", correct = FALSE)
prop.test(mat, alternative = "two.sided", correct = FALSE)

# T22
mat<-cbind(matrix(table(dataPropreP1$T22)), matrix(table(dataPropreP2$T22)))
prop.test(mat, alternative = "less", correct = FALSE)
prop.test(mat, alternative = "greater", correct = FALSE) #0.07 p-value
prop.test(mat, alternative = "two.sided", correct = FALSE)

# T23
mat<-cbind(matrix(table(dataPropreP1$T23)), matrix(table(dataPropreP2$T23)))
prop.test(mat, alternative = "less", correct = FALSE)
prop.test(mat, alternative = "greater", correct = FALSE)
prop.test(mat, alternative = "two.sided", correct = FALSE)

# T31
mat<-cbind(matrix(table(dataPropreP1$T31)), matrix(table(dataPropreP2$T31)))
prop.test(mat, alternative = "less", correct = FALSE)
prop.test(mat, alternative = "greater", correct = FALSE)
prop.test(mat, alternative = "two.sided", correct = FALSE)

# T32
mat<-cbind(matrix(table(dataPropreP1$T32)), matrix(table(dataPropreP2$T32)))
prop.test(mat, alternative = "less", correct = FALSE)
prop.test(mat, alternative = "greater", correct = FALSE)
prop.test(mat, alternative = "two.sided", correct = FALSE)

# T51
mat<-cbind(matrix(table(dataPropreP1$T51)), matrix(table(dataPropreP2$T51)))
prop.test(mat, alternative = "less", correct = FALSE)
prop.test(mat, alternative = "greater", correct = FALSE)
prop.test(mat, alternative = "two.sided", correct = FALSE)

# T52
mat<-cbind(matrix(table(dataPropreP1$T52)), matrix(table(dataPropreP2$T52)))
prop.test(mat, alternative = "less", correct = FALSE)
prop.test(mat, alternative = "greater", correct = FALSE)
prop.test(mat, alternative = "two.sided", correct = FALSE)



# keeping initial environment
rm(list=setdiff(ls(), c("dataPropre", "dataSum", "dataVec")))
