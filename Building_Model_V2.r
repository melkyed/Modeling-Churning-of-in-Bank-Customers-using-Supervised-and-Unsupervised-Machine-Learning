# Model Building
#Partition/Splitting the data.
library(MASS)
library(dplyr)
library(rpart)  # Partitioning library
library(caret) # splitting data
library(ROSE)
library(car) # mixed effect
library(randomForest)
library(ROSE)
library("rpart.plot")
library("caTools")  
library("data.tree") 
library("randomForest")
library(reshape)
library(ggraph)
library(igraph)
library(gains)
library(cluster)
library(caretEnsemble)
library(ROCR) #ROC graphs, sensitivity/specificity curves, lift charts, and precision/recall plots
#are popular examples of trade-off visualizations for specific pairs of performance measures.
#ROCR is a flexible tool for creating cutoff-parameterized 2D performance curves
#by freely combining two from over 25 performance measures (new performance measures can be added using a standard interface). 


# we can use BankChurn_sig(its may be safe to use since there is no values that much deviate) or BankChurn_sig2(the one with the transformed values)

set.seed(12)
# overview before split
#LogReg1=glm(Exited~., data = BankChurn_sig, family = "binomial")
# or LogReg1=glm(Exited~., data = BankChurn_sig, family = binomial(link="logit"))
#summary(LogReg1)

set.seed(1212)
# since the number of observation in our dataset relatively small lets go for 70/30(15,15)
# to be inline with the group work 75/25 considered
spliter=createDataPartition(BankChurn_sig$Exited,times = 1,p=0.75,list = FALSE)
Train=BankChurn_sig[spliter,]
Test=BankChurn_sig[-spliter,]
create
prop.table(table(Train$Exited))


#****************************************************************************#
#..Logistic Regression
..........................................................................#
#****************************************************************************#
# Simple

LogReg2=glm(Exited~., data = Train, family = "binomial")
# or LogReg2.1=glm(Exited~., data = Train, family = binomial(link="logit"))
#rm(LogReg2)
summary(LogReg2)

step(LogReg2, direction = "both") # stepwise
# To recheck multicollinearity using variance inflation factor
car::vif(LogReg2)
#No problem of multicollinearity.
#Pred_LogReg2.1=predict(LogReg2, Test)
#table(Train$Exited)

options(scipen = 9999)
LOR=data.frame(LogOfOdds=round(exp(coef(LogReg2)),3))
LOR$Variable=row.names(LOR)
row.names(LOR)=NULL
LOR=LOR[,c(2,1)]
LOR
write.csv(LOR, "LogOfOddRatio.csv")
#variables with more than 50% probability of changing the decision of the customer for every
#1 unit change in the respective independent variable
LOR%>%arrange(desc(LogOfOdds))%>%filter(LogOfOdds>=1)%>%mutate(Probability=round(LogOfOdds/(1+LogOfOdds),3))

#**
Pred_LogReg2=predict(LogReg2, Test, type = "response")

##model evaluation just to have a better clue whats going on 
roc.curve(Test$Exited,Pred_LogReg2)

# Advanced logistic


LogReg2=glm(Exited~., data = Train, family = "binomial")
summary(LogReg2)
step(LogReg2, direction = "both") # stepwise


# To recheck multicollinearity using variance inflation factor
car::vif(LogReg2)
#No problem of multicollinearity.

Pred_LogReg2=predict(LogReg2, Test, type = "response")
options(scipen = 9999)
LOR=data.frame(LogOfOdds=round(exp(coef(LogReg2)),3))
LOR$Variable=row.names(LOR)
row.names(LOR)=NULL
LOR=LOR[,c(2,1)]

#variables with more than 50% probability of changing the decision of the customer for every
#1 unit change in the respective independent variable
LOR%>%arrange(desc(LogOfOdds))%>%filter(LogOfOdds>=1)%>%mutate(Probability=round(LogOfOdds/(1+LogOfOdds),3))


##model evaluation just to have a better clue whats going on 
roc.curve(Test$Exited,Pred_LogReg2)



Pred.Storage=prediction(Pred_LogReg2,Test$Exited)



#plot Specificity
Specifity_Perf <- performance(prediction.obj = Pred.Storage,
                              measure="spec",
                              x.measure="cutoff")
plot(Specifity_Perf)
abline(v=c(0.20,0.24,0.28,0.32), col=c("red","green","black","pink"))

#plot Sensitivity
#par(new=TRUE)
Sensitivity_perf <- performance(prediction.obj = Pred.Storage,
                                measure="sens",
                                x.measure="cutoff")
plot(Sensitivity_perf)

perfo=performance(Pred.Storage,"tpr","fpr")

plot(perfo)

###########################
options(scipen = 999)
cut_offs=data.frame(cut=perfo@alpha.values[[1]], fpr=perfo@x.values[[1]], tpr=perfo@y.values[[1]])
cut_offs=cut_offs[order(cut_offs$tpr, decreasing = TRUE),]

cut_offs%>%filter(fpr<=0.4,tpr>=0.6)
#           cut    fpr   tpr
#    0.1683683 0.3979798 0.8057692
#cutoff of 16 seems to give the highest tpr and relatively low fpr

Pred.class=ifelse(Pred_LogReg2>0.16,1,0)
CM=confusionMatrix(as.factor(Pred.class),as.factor(Test$Exited))
fourfoldplot(CM$table)
Acc_Log16=CM$overall[[1]]
Sensitivity_Log16=CM$byClass[[1]]
Specificity_Log16=CM$byClass[[2]]

#Choice of cutoff at 18,22,24,26 with increasing accuracy and sensitivity 
#but decreasing Specificity
AUC=performance(Pred.Storage,"auc")
AUC_Log16=AUC@y.values[[1]]
F1sq_Log16=CM$byClass[[7]]


############
Pred.class=ifelse(Pred_LogReg2>0.18,1,0)
CM=confusionMatrix(as.factor(Pred.class),as.factor(Test$Exited))
fourfoldplot(CM$table)
Acc_Log18=CM$overall[[1]]
Sensitivity_Log18=CM$byClass[[1]]
Specificity_Log18=CM$byClass[[2]]
F1sq_Log18=CM$byClass[[7]]
AUC=performance(Pred.Storage,"auc")
AUC_Log18=AUC@y.values[[1]]
################################

Pred.class=ifelse(Pred_LogReg2>0.22,1,0)
CM=confusionMatrix(as.factor(Pred.class),as.factor(Test$Exited))
fourfoldplot(CM$table)
Acc_Log22=CM$overall[[1]]
Sensitivity_Log22=CM$byClass[[1]]
Specificity_Log22=CM$byClass[[2]]
F1sq_Log22=CM$byClass[[7]]
#Choice of cutoff at 22,24, 26 with increasing accuracy and sensitivity but decreasing Specificity
AUC=performance(Pred.Storage,"auc")
AUC_Log22=AUC@y.values[[1]]

# model tunning
Pred_LogReg2.class=ifelse(Pred_LogReg2>0.24,1,0)
CM=confusionMatrix(as.factor(Pred_LogReg2.class),as.factor(Test$Exited))
CM$table
fourfoldplot(CM$table)
Acc_Log24=CM$overall[[1]]
Sensitivity_Log24=CM$byClass[[1]]
Specificity_Log24=CM$byClass[[2]]
F1sq_Log24=CM$byClass[[7]]

AUC=performance(Pred.Storage,"auc")
AUC_Log24=AUC@y.values[[1]]
#lda(linear discriminanat analysis)


##################################
#names(Train)

LDA_p60=lda(Exited~., data = Train,prior=c(0.60,0.40))
Pred_LogReg3=predict(LDA_p60,Test)
CM=confusionMatrix(as.factor(Pred_LogReg3$class),as.factor(Test$Exited))
Acc_LDA_60=CM$overall[[1]]
Sensitivity_LDA60=CM$byClass[[1]]
Specificity_LDA60=CM$byClass[[2]]
Pred.Storage=prediction(as.numeric(Pred_LogReg3$class),Test$Exited)
AUC=performance(Pred.Storage,"auc")
AUC_LDA60=AUC@y.values[[1]]
F1sq_LDA60=CM$byClass[[7]]
####################
LDA_p58=lda(Exited~., data = Train,prior=c(0.58,0.42))
PredLDA58=predict(LDA_p58,Test)
CM=confusionMatrix(as.factor(PredLDA58$class),as.factor(Test$Exited))
Acc_LDA_58=CM$overall[[1]]
Sensitivity_LDA58=CM$byClass[[1]]
Specificity_LDA58=CM$byClass[[2]]
Pred.Storage=prediction(as.numeric(PredLDA58$class),Test$Exited)
AUC=performance(Pred.Storage,"auc")
AUC_LDA58=AUC@y.values[[1]]
F1sq_LDA58=CM$byClass[[7]]
#######################################
LDA_p70=lda(Exited~., data = Train,prior=c(0.70,0.30))
Pred_LogReg3=predict(LDA_p70,Test)
CM=confusionMatrix(as.factor(Pred_LogReg3$class),as.factor(Test$Exited))
Acc_LDA_70=CM$overall[[1]]
Sensitivity_LDA70=CM$byClass[[1]]
Specificity_LDA70=CM$byClass[[2]]
F1sq_LDA70=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred_LogReg3$class),Test$Exited)
AUC=performance(Pred.Storage,"auc")
AUC_LDA70=AUC@y.values[[1]]
#############
#/*LDA_p52=lda(Exited~., data = Train,prior=c(0.52,0.48))
#Pred_LogReg2=predict(LDA_p52,Test)
#CM=confusionMatrix(Pred_LogReg2$class,Test$Exited)
#Acc_LDA_52=CM$overall[[1]]
#Sensitivity_LDA52=CM$byClass[[1]]
#Specificity_LDA52=CM$byClass[[2]]
#F1sq_LDA52=CM$byClass[[7]]
#Pred.Storage=prediction(as.numeric(Pred_LogReg2$class),Test$Exited)
#AUC=performance(Pred.Storage,"auc")
#AUC_LDA52=AUC@y.values[[1]]


# token


###############************Decission Tree *****************#################
# Train Decision Tree 
LogReg2DT=rpart(Exited ~., data = Train, method="class")
# or LogReg2DT=rpart(Exited ~ CreditScore + Geography + Gender + Age + EstimatedSalary+ Balance + NumOfProducts, data = Train)
LogReg2DT
summary(LogReg2DT)
# Validate Decision Tree
LogReg2DT.predicted <- predict(LogReg2DT, Test, type='class')

# Generate COnfusion Matrix
# The confusion Matrix function provides error calculations automatically
confusionMatrix(LogReg2DT.predicted , as.factor(Test$Exited))

# you can also use the simple table function to display the confusion matrix...
table(LogReg2DT.predicted, as.factor(Test$Exited))

# Decision Tree Visualization
prp(LogReg2DT)

# Another function to visualize the decision tree with colors.
rpart.plot(LogReg2DT, extra=4)
# Experiment with various settings for function rpart.plot
rpart.plot(LogReg2DT, type = 2, extra=101)

################
# extra for model comparison
CM=confusionMatrix(LogReg2DT.predicted,as.factor(Test$Exited))
CM$table
Acc_DT1=CM$overall[[1]]
Sensitivity_DT1=CM$byClass[[1]]
Specificity_DT1=CM$byClass[[2]]

F1sq_DT1=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(LogReg2DT.predicted),Test$Exited)
AUC=performance(Pred.Storage,"auc")
AUC_DT1=AUC@y.values[[1]]



###############
#***Simple RF

#model_rf1 <- randomForest(as.factor(Exited) ~., data = Train)

#model_rf2 <- randomForest(Exited ~., data = Train)

#summary(model_rf1)
#model_rf1.predicted <- predict(model_rf1, Test)


# Step 3:  Evaluate the models.

# Create the ROC Curve
roc.curve(Test$Exited,Pred_LogReg2)
roc.curve(Test$Exited,LogReg2DT.predicted)
roc.curve(Test$Exited,model_rf1.predicted)

#*********************************************RR

########*************** Adv. RandomForest************############

#the target variable has only about 20% of the levels as 1 and hence randomforest model
#gets negatively affected by the imbalance. SMOTE technique is used to have more
#examples of the less represented level of the target variable
set.seed(12121212)

d.Rose=ROSE(Exited~., data = Winsorized, seed = 1)$data
prop.table(table(d.Rose$Exited))
dim(Winsorized)
set.seed(1221)
Index=createDataPartition(d.Rose$Exited, p=0.75, list = FALSE, times = 1)
Train.R=d.Rose[Index,]
Test.R=d.Rose[-Index,]

set.seed(212121)
RF=randomForest(as.factor(Exited)~., data = Train.R)
RF1=randomForest(as.factor(Exited)~., data = Train.R)
randomForest::varImpPlot(RF)
Pred_RF=predict(RF,Test.R)
CM=confusionMatrix(Pred_RF,as.factor(Test.R$Exited))
CM$table
Acc_RF1=CM$overall[[1]]
Sensitivity_RF1=CM$byClass[[1]]
Specificity_RF1=CM$byClass[[2]]

F1sq_RF1=CM$byClass[[7]]
Pred.Storage=prediction(as.numeric(Pred_RF),Test$Exited)
AUC=performance(Pred.Storage,"auc")
AUC_RF1=AUC@y.values[[1]]
##############



###################################################################
######################### Model evaluation ########################
###################################################################


Compare=data.frame(Models=c("LDA58","LDA60","LDA70","LogReg2DT","RF1"),
                   Accuracy=round(c(Acc_Log16,Acc_Log18,Acc_Log22,Acc_Log24,Acc_LDA_58,Acc_LDA_60,Acc_LDA_70,Acc_DT1,Acc_RF1),2),
                   Sensitivity=round(c(Sensitivity_Log16,Sensitivity_Log18,Sensitivity_Log22,Sensitivity_Log24,Sensitivity_LDA70,Sensitivity_LDA58,Sensitivity_LDA60,Sensitivity_DT1, Sensitivity_RF1),2),
                   Specificity=round(c(Specificity_Log16,Specificity_Log18,Specificity_Log22,Specificity_Log24,Specificity_LDA58,Specificity_LDA60,Specificity_LDA70,Specificity_DT1, Specificity_RF1),2),
                   AUC=round(c(AUC_Log16,AUC_Log18,AUC_Log22,AUC_Log24,AUC_LDA58,AUC_LDA60,AUC_LDA70,AUC_DT1, AUC_RF1),2),
                   F1=round(c(F1sq_Log16,F1sq_Log18,F1sq_Log22,F1sq_Log24,F1sq_LDA58,F1sq_LDA60,F1sq_LDA70,F1sq_DT1, F1sq_RF1),2))

Compare
##############

ggplot(melt(Compare,id.vars = "Models"),aes(Models,value, col=variable, group=variable))+geom_line()+
  geom_point(size=5,shape=21,fill="white")+scale_y_continuous(breaks = seq(0,1,0.25))+
  labs(x="",y="Values", title="Evaluation Metric Comparison", color="Metrics")+
  theme(legend.key = element_rect(colour = "black", fill = "light blue"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#################




# Kmean clustering
BankChurn_cluster= subset(original_data, select =-c(CustomerId, IsActiveMember,Surname, RowNumber))
str(BankChurn_cluster)
# scaling var

#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = BankChurn_cluster,fullRank = T)
BankChurn_cluster <- data.frame(predict(dmy, newdata = BankChurn_cluster))

BankChurn_cluster_normalized <- scale(BankChurn_cluster[1:10])


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(BankChurn_cluster_normalized, nc=8)


set.seed(12012)
clusters_num = 3
k.means.fit <- kmeans(BankChurn_cluster_normalized, clusters_num,iter.max = 1000)

k.means.fit


clusplot(BankChurn_cluster_normalized, k.means.fit$cluster, main='',
         color=TRUE, shade=FALSE,
         labels=clusters_num, lines=0)


centers <- table(BankChurn_cluster[,11],k.means.fit$cluster)

centers


#Let’s try to figure out how  clusters 1 and 2 differ from each other. 
#Code below calculates a difference vector of cluster 1 and 2 and sorts the attributes in the order of the most influence.

Difference <- k.means.fit$centers[2,] - k.means.fit$centers[3,]
Difference <- Difference[order(abs(Difference), decreasing = T)]
Difference


# with diffrent k=6
set.seed(1420)
clusters_num = 6
k.means.fit <- kmeans(BankChurn_cluster_normalized, clusters_num,iter.max = 1000)
k.means.fit

clusplot(BankChurn_cluster_normalized, k.means.fit$cluster, main='',
         color=TRUE, shade=FALSE,
         labels=clusters_num, lines=0)

centers <- table(BankChurn_cluster[,11],k.means.fit$cluster)

centers


#how clusters 3 and 4 differ from each other. 
#Code below calculates a difference vector of cluster 1 and 3 and the sort the attributes in the order of the most influence:
Difference <- k.means.fit$centers[1,] - k.means.fit$centers[3,]
Difference <- Difference[order(abs(Difference), decreasing = T)]
Difference












# to chiise targeted customers with thier respected ID/or what ever identification we have
library(gains)
Target_customers=Winsorized

# here u can generate random cust ID since the bank data is anonymus/in real scenario u can use actual c.ID 
set.seed(121212)
Index=createDataPartition(Target_customers$Exited,times = 1,p=0.75,list = FALSE)
Train_target=Target_customers[Index,]
Test_target=Target_customers[-Index,]

Logtoselect=glm(formula = Exited ~. , family = "binomial", 
                data = Target_customers)

gains(as.numeric(Target_customers$Exited),predict(Logtoselect,type="response",
                                                  newdata=Target_customers),groups = 10)
Target_customers$Cust_ID<-original_data$CustomerId
Target_customers$prob<-predict(Logtoselect,type="response",newdata=Target_customers)
quantile(Target_customers$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
targeted=Target_customers%>%filter(prob>0.32575619 & prob<=0.70000452)%>%dplyr::select(Cust_ID)
dim(targeted)
write.csv(targeted,"targetedCustomers.csv")
##############################



