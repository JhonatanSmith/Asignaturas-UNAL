###################ALGUNOS PROGRAMAS CLASE 10############################

library(ISLR)
library(MASS)
library(FNN)
library(ggplot2)
library(wesanderson)

Defaulted=ISLR::Default
Balance=Defaulted$balance
Income=Defaulted$income
levels(Defaulted$default) <- c(0,1)
Default=as.integer(Defaulted$default)-1

#Using OLS

lm.fit=lm(Default~Balance)
par(mfrow=c(1,2))
plot(Balance,lm.fit$fitted.values,type="l",lwd=2,
     col="lightblue",ylim=c(-0.1,1),xlab="Balance",ylab="Probabilty of Default",
     main="Linear Regression")
abline(h=0,col="orange",lty=2,lwd=2)
abline(h=1,col="orange",lty=2,lwd=2)
df=data.frame(Default,Balance)

#Using LR

lr.fit=glm(Default~Balance, data = df, family=binomial)
x<-seq(min(Balance),max(Balance),length.out=length(Balance))
pred<-predict(lr.fit,data.frame(Balance=x),type="resp")
plot(Balance,Default,pch=20,cex=0.2,col="white",ylim=c(-0.1,1),
     xlab="Balance",ylab="Probability of Default",main="Logistic regression")
lines(x,pred,,lwd=2,col="lightblue")
abline(h=0,col="orange",lty=2,lwd=2)
abline(h=1,col="orange",lty=2,lwd=2)
###########################################################################
library(ISLR)
library(MASS)
library(FNN)
library(ggplot2)
library(wesanderson)
library(Amelia)
Defaulted=ISLR::Default
missmap(Defaulted, main = "Missing values vs observed")
Balance=Defaulted$balance
Income=Defaulted$income
Student=Defaulted$student
levels(Defaulted$default) <- c(0,1)
Default=as.integer(Defaulted$default)-1
df=data.frame(Default,Balance)
lr.fit=glm(Default~Balance, data = df, family=binomial)
summary(lr.fit)$coefficients


###########################################################################
library(ISLR)
library(MASS)
library(FNN)
library(ggplot2)
library(wesanderson)
library(Amelia)
Defaulted=ISLR::Default
missmap(Defaulted, main = "Missing values vs observed")
Balance=Defaulted$balance
Income=Defaulted$income
Student=Defaulted$student
levels(Defaulted$default) <- c(0,1)
Default=as.integer(Defaulted$default)-1
df=data.frame(Default,Balance,Income,Student)
lr.fit=glm(Default~Balance+Student, data = df, family=binomial)
summary(lr.fit)$coefficients

###########################################################################
library(ISLR)
library(MASS)
library(wesanderson)
mu_1=-1.25
mu_2=1.25
sigma=1
x<-seq(-4,4,length.out=100)
fk_1=(1/(sqrt(2*pi)*sigma))*exp(-(1/(2*sigma^2))*(x-mu_1)^2)
fk_2=(1/(sqrt(2*pi)*sigma))*exp(-(1/(2*sigma^2))*(x-mu_2)^2)
set.seed(1234)
rs_1=rnorm(20,-1.25,sd=1)
rs_2=rnorm(20,1.25,sd=1)
LDA_db=(mean(rs_1)+mean(rs_2))/2
par(mfrow=c(1,2))

plot(x,fk_1,pch=1,cex=0.1,type="l",col="limegreen",lwd=2,yaxt = "n",ylab="",xlab="")
lines(x,fk_2,pch=1,cex=0.1,col="pink",lwd=2)
abline(v=0,lty=2,lwd=2)

hist(rs_1,col="limegreen",nclass=6,xlim=c(-3,4),border="limegreen",ylab="",xlab="",main="",density=55)
hist(rs_2,col="pink",add=TRUE,nclass=6,border="pink2",transparent=TRUE,density=45)
abline(v=0,lty=2,lwd=1)
abline(v=LDA_db,lty=1,lwd=1)
legend("topright",legend=c("Bayes db","LDA db"),lty=c(2,1),cex=0.5)

###########################################################################
#Bayes error rate and LDA error rate. 

library(ISLR)
library(MASS)
library(wesanderson)
library(shape)
library(nnet)

mu_1=-1.25
mu_2=1.25
sigma=1
nk=150
set.seed(12)
rs_1=rnorm(nk,mu_1,sd=sigma)
g1<-rep(1,length(rs_1))
m1<-cbind(rs_1,g1)
rs_2=rnorm(nk,mu_2,sd=sigma)
g2<-rep(2,length(rs_2))
m2<-cbind(rs_2,g2)
df=data.frame(x=c(rs_1,rs_2),y=c(g1,g2))

bayesdelta_k<-function(x,mu_k,sigma,prior)
{dk<-x*(mu_k/sigma^2)-(mu_k^2/(2*sigma^2))+log(prior)
;dk
}

## 60% of the sample size

smp_size <- floor(0.6 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
train<-train[,1:2]
test <- df[-train_ind, ]
test<-data.frame(test[,1:2])
######BAYES
d1<-rep(NA,nrow(test))
for(i in 1:nrow(test)){
  d1[i]<-bayesdelta_k(test[i,1],mu_1,sigma,0.5)
}

d2<-rep(NA,nrow(test))
for(i in 1:nrow(test)){
  d2[i]<-bayesdelta_k(test[i,1],mu_2,sigma,0.5)
}
test1<-data.frame(test,d1,d2)
maximos<-apply(test1[,3:4],1,max)
newtest<-data.frame(test1,maximos)
bayes_class<-ifelse(newtest$maximos==newtest$d1,1,2)
t<-ifelse(test$y==bayes_class,0,1)
Bayes_err<-sum(t)/length(t)

######LDA
model <- lda(y ~., data=train)
test_yhats<-predict(model,test,type=c("class"))$class
test_lda<-ifelse(test$y==test_yhats,0,1)
LDA_test_err<-sum(test_lda)/nrow(test)

Bayes<-table(test$y,bayes_class)
Lda<-table(test$y,test_yhats)

list(Bayes=Bayes,Lda=Lda,Bayes_err=Bayes_err,
     LDA_test_err=LDA_test_err)

###########################################################################
library(ISLR)
library(MASS)
library(wesanderson)
library(shape)

grid=60

x1<-seq(-3,3,length.out = grid)
x2<-seq(-3,3,length.out = grid)
mean_1=c(0,0)
mean_2=c(0,0)
Sigma_1=matrix(c(1,0,0,1),2,2)
Sigma_2=matrix(c(1,0.7,0.7,1),2,2)

xy <- expand.grid( x = x1, y = x2)

f1_x=function(x1,x2){z<-(1/(2*pi*det(Sigma_1)))*
  exp(-0.5*(matrix(c(x1-mean_1[1],x2-mean_1[2]),1,2)%*%
              ginv(Sigma_1)%*%matrix(c(x1-mean_1[1],x2-mean_1[2]),2,1)
  ));z
}

f2_x=function(x1,x2){z<-(1/(2*pi*det(Sigma_2)))*
  exp(-0.5*(matrix(c(x1-mean_2[1],x2-mean_2[2]),1,2)%*%
              ginv(Sigma_2)%*%matrix(c(x1-mean_2[1],x2-mean_2[2]),2,1)
  ));z
}

z1<-rep(NA,nrow(xy))

for(i in 1:nrow(xy)){
  z1[i]<-f1_x(xy[i,1],xy[i,2])
}
z1<-matrix(z1,grid,grid)

####################

z2<-rep(NA,nrow(xy))

for(i in 1:nrow(xy)){
  z2[i]<-f2_x(xy[i,1],xy[i,2])
}
z2<-matrix(z2,grid,grid)

par(mfrow=c(1,2))

persp(x1,x2,z1,col=drapecol(z1),phi=35,theta=30,expand=0.4,shade=0.001,border=NA)

persp(x1,x2,z2,col=drapecol(z2),phi=35,theta=30,expand=0.4,shade=0.001,border=NA)

###########################################################################
library(ISLR)
library(MASS)
library(wesanderson)
Defaulted=ISLR::Default
Balance=Defaulted$balance
Income=Defaulted$income
Student=Defaulted$student
levels(Defaulted$default) <- c(0,1)
Default=as.integer(Defaulted$default)-1
df<-data.frame(Default,Balance,Income,Student)
lda.fit=lda(Default~., data = df)
lda.class<-predict(lda.fit,df,type=c("class"))$class

Confussion_Matrix<-table(Pred_Default=lda.class,True_Default=df$Default)
train_lda<-ifelse(df$Default==lda.class,0,1)
LDA_train_err<-sum(train_lda)/nrow(df)
list(Confussion_Matrix=Confussion_Matrix,LDA_train_err=LDA_train_err)

###########################################################################
library(ISLR)
library(MASS)
library(wesanderson)
Defaulted=ISLR::Default
Balance=Defaulted$balance
Income=Defaulted$income
Student=Defaulted$student
levels(Defaulted$default) <- c(0,1)
Default=as.integer(Defaulted$default)-1
df<-data.frame(Default,Balance,Income,Student)
lda.fit=lda(Default~., data = df)
posterior<-predict(lda.fit,df[,2:4],type=c("posterior"))$posterior[,2]
summary(posterior)
length(posterior)
lda.class<-ifelse(posterior>0.2,1,0)
table(lda.class)

Confussion_Matrix<-table(Pred_Default=lda.class,True_Default=df$Default)
train_lda<-ifelse(df$Default==lda.class,0,1)
LDA_train_err<-sum(train_lda)/nrow(df)
list(Confussion_Matrix=Confussion_Matrix,LDA_train_err=LDA_train_err)

########################Material relacionado con ROC Curve###################
#MODERN LOGISTIC REGRESSION IN R. STATISTICS IN THE INTERNET AGE
#INTRODUCTION TO ANALYTICS. SCHOOL OF STATISTICS. UNALMED
#Adapted from: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

library(MASS)
library(Amelia)
library(pscl)
library(caret)
library(pROC)
library(ROCR)

#STEPS TO PERFORM LR IN THE COMPUTER AGE
#1) The data cleaning process
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""),sep=";")

sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
missmap(training.data.raw, main = "Missing values vs observed")
data <- subset(training.data.raw,select=c(1,2,4,5,6,7,9,11))
data$fare<-as.numeric(paste(data$fare))
data$fare[is.na(data$fare)] <- mean(data$fare,na.rm=T)
data$age[is.na(data$age)] <- mean(data$age,na.rm=T)
data <- data[!is.na(data$embarked),]
rownames(data) <- NULL
par(mfrow=c(2,2))

boxplot(data$age~data$survived,col=c("blue","orange"),xlab="Survived",ylab="Age")
boxplot(data$fare~data$survived,col=c("blue","orange"),xlab="Survived",ylab="Fare")

boxplot(data$age~data$sex,col=c("blue","orange"),xlab="Sex",ylab="Age")
boxplot(data$fare~data$sex,col=c("blue","orange"),xlab="Sex",ylab="Fare")
sex_by_survived<-table(data$sex,data$survived)
embarked_by_survived<-table(data$embarked,data$survived)
Age_summary<-summary(data$age)
Fare_summary<-summary(data$fare)

list(sex_by_survived=sex_by_survived,
     embarked_by_survived=embarked_by_survived,
     Age_summary=Age_summary,
     Fare_summary=Fare_summary)
#3) Model fitting
#We split the data into two chunks: training and testing set. 
#The training set will be used to fit our model which we will be testing over the testing set.
train <- data[1:800,]
test <- data[801:889,]

model <- glm(survived ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
pR2(model)

#4) Assessing the predictive ability of the model
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')

# fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results <- as.factor(ifelse(fitted.results > 0.5,1,0))

misClasificError <- mean(fitted.results != test$survived)
print(paste('Accuracy',1-misClasificError))
#5) Confussion Matrix
confusionMatrix(data=fitted.results, reference=as.factor(test$survived))
#6) Assesing the performance of the classifier: Curva ROC.
par(mfrow=c(1,1))
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
plot(roc(test$survived, p, direction="<"),
     col="purple2", lwd=3, main="ROC curve")
#Area under ROC curve (ROC statistic)
pr <- prediction(p, test$survived)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
list(AUC=auc)

