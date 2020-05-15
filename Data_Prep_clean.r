
#**********************************York University********************************#
#*****Lab2 Submition for the course CSDA1010 Basic Methods of Data Analytics******# 

#******* Modeling customer churn at Banking Industry  *******#
#******  Date 26/04/2020 **** #

# Read dataset
getwd()
BankChurn=read.csv("Churn_Modelling.csv")

# the dataset shared at "https://github.com/melkyed/Modeling-Churning-of-in-Bank-Customers-using-Supervised-and-Unsupervised-Machine-Learning/blob/master/Churn_Modelling.csv"

original_data=BankChurn
# libraries to be used
library(dplyr)
library(corrplot)
library(ranger) # required by Boruta
library(Boruta) # feature engineering selection
library(DescTools) # to use Winsorize
#library(randomForest)



# cleaning data
dim(BankChurn)
str(original_data)

# Remove unnecessary var
BankChurn= subset(BankChurn, select =-c(CustomerId, IsActiveMember,Surname, RowNumber))
str(BankChurn)

# checking for missing
anyNA(BankChurn) 
# no missing value no need to go further to deal with missings
#Percentage_missing=round(colSums(is.na(BankChurn[,colnames(BankChurn)[colSums(is.na(BankChurn))>0]]))/nrow(BankChurn)*100,2)
#data.frame(MissingProportion=sort(Percentage_missing, decreasing = TRUE))

#Finding variable names with more than 25% missing values
#Variables_with_High_NAs=colnames(BankChurn)[colSums(is.na(BankChurn))/nrow(BankChurn)>0.25]
#Variables_with_High_NAs
#luckily we dont have variable/s with more than 25% missing value

#Finding variable names with more than 15% missing values
#Variables_with_High_NAs=colnames(BankChurn)[colSums(is.na(BankChurn))/nrow(BankChurn)>0.15]
#Variables_with_High_NAs
# we dont have variable/s with more than 15% missing value too 
# safe to go with out removing any of our candidate variables

#*************Other EAs ***************#
#to be taken from group results

#********************************#


#Feature Selection 
#Here we can use boruta library to estimate variable importance which leads to remove insignificant variables
#The Boruta algorithm is a wrapper(like forward and backward elimination method) built around the random forest classification algorithm.
#It tries to capture all the important, interesting features you might have in your dataset
#with respect to an outcome variable.


# lets initialize a seed for reproducibility
set.seed(12)     

# Simulating using Boruta algorithm to identify insignificant var in bank churn data (BBCD)
# very useful especially if there is a large number of independent variables involved
BBCD=Boruta(Exited~., data = na.omit(BankChurn), doTrace=2) # here we used na omit just to be sure as the algorithm in boruta will throw error in presence of missings

print(BBCD) # out put from Boruta bank churn data train
# Boruta performed 38 iterations in 13.46781 mins.
#2 attributes confirmed unimportant: HasCrCard, Tenure;
#The number of variables left is 7
getSelectedAttributes(BBCD)
# Save the significant variables in separate file
write.csv(getSelectedAttributes(BBCD),"Selected_Variables.csv")
write.csv(getSelectedAttributes(BBCD),"SignificantVariables")
getSelectedAttributes(BBCD)

Sig_Variables=c("CreditScore","Geography","Gender", "Age","Balance", "NumOfProducts", "EstimatedSalary", "Exited")

#Dataset with only the significant variables
BankChurn_sig=BankChurn[,Sig_Variables]
str(BankChurn_sig)

#Checking missing after screening if imputation performed in our case safe to go
#Percentage_missing=round(colSums(is.na(BankChurn_sig[,colnames(BankChurn_sig)[colSums(is.na(BankChurn_sig))>0]]))/nrow(BankChurn_sig)*100,2)
#sort(Percentage_missing, decreasing = TRUE)
#once again we confirmed we dont have any missing variable

#checking for correlated variables
Numeric=Filter(is.numeric,BankChurn_sig)

M=cor(na.omit(Numeric))
corrplot(M, method = "circle", type = "lower", 
         tl.srt = 45, tl.col = "black", tl.cex = 0.75)

#No problem with multicolinearity good to go to the next step
# we dont even need to check a significance test for obvious reason

# checking for Outlier/s
Numeric=Filter(is.numeric,BankChurn_sig)

y=names(Numeric)
for (i in 1:length(y) ){
  x=boxplot(Numeric[,y[i]]~BankChurn_sig$Exited,
            main=y[i], col = i ,ylab="Exited",horizontal = TRUE)
  index=which(BankChurn_sig[,y[i]]%in%x$out)
  rm(x)
}

#Adjusting the outlier values observed in Age, CreditScore and NumOfProducts
# Winsorize (Replace Extreme Values by Less Extreme Ones)
# Winsorizing a vector means that a predefined quantum of the smallest and/or the largest
#values are replaced by less extreme values. 
#Thereby the substitute values are the most extreme retained values.
# small to severe outlier detected in NumOfProducts(only 1 outlier), CreditScore, Age in ascending order

# For outlier treatment, Winsorize all numeric variables with extreme outlier and then log transform

Winsorized=data.frame(lapply(Numeric,Winsorize))
#table(BankChurn_sig$NumOfProducts)
Winsorized$Age=log(Winsorized$Age)
Winsorized$CreditScore=log(Winsorized$CreditScore)
Winsorized$NumOfProducts=log(Winsorized$NumOfProducts)

#lets create a new object to pass the transformed variables BankChurn_sig2

Num2=Filter(is.numeric,Winsorized)
y2=names(Num2)
for (i in 1:length(y2) ){
  x=boxplot(Num2[,y2[i]]~BankChurn_sig$Exited,
            main=y2[i], col = i ,ylab="Exited",horizontal = TRUE)
  index=which(BankChurn_sig[,y2[i]]%in%x$out)
  rm(x)
}

# the dataset BankChurn_sig2 is based on the transformed vakue of Age, CreditScore,NumOfProducts
BankChurn_sig2=read.csv("BankChurn_sig2.csv")
BankChurn_sig2= subset(BankChurn_sig2, select =-c(X))

# we have NumOfProducts, CreditScore, Age adjusted by log transformation and passed to BankChurn_sig2


