
#********************************************************************#
#*************************EA and Visualization **********************#
#*************************Univariate Analysis **********************#
#********************************************************************#
library(ggplot2)
library(reshape)

# we can use BankChurn_sig(its may be safe to use since there is no values that much deviate) or BankChurn_sig2(the one with the transformed values)
#BankChurn_sig2=Winsorized
a=ggplot(BankChurn_sig2, aes(x=CreditScore, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - CreditScore Vs Exited")+
  theme_bw()+facet_grid(~Exited)+theme(plot.title = element_text(size = 10, hjust = 0.5))

s1=aggregate(BankChurn_sig2$CreditScore, by=list(BankChurn_sig2$Exited) , FUN = summary)
print(s1)



a=ggplot(BankChurn_sig2, aes(x=Age, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 15)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - Age Vs Exited")+
  theme_bw()+facet_grid(~Exited)+theme(plot.title = element_text(size = 10, hjust = 0.5))

s2=aggregate(BankChurn_sig2$Age, by=list(BankChurn_sig2$Exited) , FUN = summary)
print(s2)

a=ggplot(BankChurn_sig, aes(x=NumOfProducts, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 30)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - NumOfProducts Vs Exited")+
  theme_bw()+facet_grid(~Exited)+theme(plot.title = element_text(size = 10, hjust = 0.5))

s3=aggregate(BankChurn_sig2$NumOfProducts, by=list(BankChurn_sig2$Exited) , FUN = summary)
print(s3)

a=ggplot(BankChurn_sig2, aes(x=Balance, y=..density..,  fill=1))
a+geom_histogram(stat = "bin", bins = 30)+geom_density(alpha=0.5)+
  guides(fill=FALSE)+labs(y="Density", title="Density graph - Balance Vs Exited")+
  theme_bw()+facet_grid(~Exited)+theme(plot.title = element_text(size = 10, hjust = 0.5))

s4=aggregate(BankChurn_sig2$Balance, by=list(BankChurn_sig2$Exited) , FUN = summary)
print(s4)
# keep in mind that we may need to transform Balance if necessary at some point
