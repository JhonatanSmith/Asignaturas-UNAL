#Ejemplos Clase 10 ISLR

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

####LR######

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

####LDA####

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

#Bayes error rate and LDA error rate. ISLR page 141

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

######

#Obtaining Figure 4.5 ISLR
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

#########

#CODE TO SIMULATE A FIGURE SIMILAR TO 4.6 ISLR BASED ON 
#BOTH BAYES CLASSIFIER AND LDA for p=2

library(MASS)
library(class)
library( mixtools )
library(caret)

set.seed(1234)
n_k=20
rho=0.75
m1<-mvrnorm(n = n_k, mu=c(-2,2), Sigma=matrix(c(1,rho,rho,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
x1=m1[,1]
x2=m1[,2]
y<-rep(1,n_k)
m1=cbind(x1,x2,y)

m2<-mvrnorm(n = n_k, mu=c(2,3), Sigma=matrix(c(1,rho,rho,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
x1=m2[,1]
x2=m2[,2]
y<-rep(2,n_k)
m2=cbind(x1,x2,y)

m3<-mvrnorm(n = n_k, mu=c(1,5.5), Sigma=matrix(c(1,rho,rho,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
x1=m3[,1]
x2=m3[,2]
y<-rep(3,n_k)
m3=cbind(x1,x2,y)
par(mfrow=c(1,2))
plot(m3[,1], m3[,2],  col="blue", pch=19, xlab="X1", ylab="X2",
     xlim=c(-5,5),ylim=c(-4,8),main='Left Panel Figure 4.6') 
ellipse(mu=c(1,5.5), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="blue",lwd=2)

points(m2[,1], m2[,2],  col="limegreen", pch=19, xlab="X1", ylab="X2") 
ellipse(mu=c(2,3), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="limegreen",lwd=2)

points(  m1[,1], m1[,2],  col="red", pch=19, xlab="X1", ylab="X2") 
ellipse(mu=c(-2,2), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="red",lwd=2)
legend("topleft",legend=c("G3","G2","G1"),col=c("blue","limegreen","red"),
       pch=c(19,19,19),cex=0.8)

df<-data.frame(rbind(m1,m2,m3))

Sigma=matrix(c(1,rho,rho,1),2,2)
mu_1=c(-2 , 2)
mu_2=c(2,3)
mu_3=c(1,5.5)
d_1<-function(x1,x2){matrix(c(x1,x2),1,2)%*%
    ginv(Sigma)%*%matrix(mu_1,2,1)-
    0.5*matrix(mu_1,1,2)%*%ginv(Sigma)%*%matrix(mu_1,2,1)+log(1/3)
}

d_2<-function(x1,x2){matrix(c(x1,x2),1,2)%*%
    ginv(Sigma)%*%matrix(mu_2,2,1)-
    0.5*matrix(mu_2,1,2)%*%ginv(Sigma)%*%matrix(mu_2,2,1)+log(1/3)
}

d_3<-function(x1,x2){matrix(c(x1,x2),1,2)%*%
    ginv(Sigma)%*%matrix(mu_3,2,1)-
    0.5*matrix(mu_3,1,2)%*%ginv(Sigma)%*%matrix(mu_3,2,1)+log(1/3)
}

####evaluating on the data

d1<-rep(NA,nrow(df))
for(i in 1:nrow(df)){
  d1[i]<-d_1(df[i,1],df[i,2])
}

d2<-rep(NA,nrow(df))
for(i in 1:nrow(df)){
  d2[i]<-d_2(df[i,1],df[i,2])
}

d3<-rep(NA,nrow(df))
for(i in 1:nrow(df)){
  d3[i]<-d_3(df[i,1],df[i,2])
}

dks1<-data.frame(d1,d2,d3)
maxdks=apply(dks1,1,max)
ndks1<-data.frame(dks1,maxdks)
ndks1$cat<-0
yhat<-ifelse(ndks1$d1==ndks1$maxdks,ndks1$cat<-1,
             ifelse(ndks1$d2==ndks1$maxdks,ndks1$cat<-2,
                    ifelse(ndks1$d3==ndks1$maxdks,ndks1$cat<-3,
                           ndks1$cat<-NA)))

####################BAYES DECISION BOUNDARIES###################

###Evaluating with all the grid and categories

df1<-df[(df[,3]==1 ),]
df2<-df[(df[,3]==2 ),]
df3<-df[(df[,3]==3 ),]

df123<-rbind(df1,df2,df3)
resolution=100
xnew1 <- seq(-5, 5, len=resolution)
ynew1 <- seq(-5, 8, len=resolution)
xnew <- expand.grid(x1 = xnew1, x2 = ynew1)
d1<-rep(NA,nrow(xnew))
for(i in 1:nrow(xnew)){
  d1[i]<-d_1(xnew[i,1],xnew[i,2])
}

d2<-rep(NA,nrow(xnew))
for(i in 1:nrow(xnew)){
  d2[i]<-d_2(xnew[i,1],xnew[i,2])
}

d3<-rep(NA,nrow(xnew))
for(i in 1:nrow(xnew)){
  d3[i]<-d_3(xnew[i,1],xnew[i,2])
}


dks123<-data.frame(d1,d2,d3)
maxdks123=apply(dks123,1,max)
ndks123<-data.frame(dks123,maxdks123)
ndks123$cat<-0
yhat123<-ifelse(ndks123$d1==ndks123$maxdks,ndks123$cat<-1,
                ifelse(ndks123$d2==ndks123$maxdks,ndks123$cat<-2,
                       ifelse(ndks123$d3==ndks123$maxdks,ndks123$cat<-3,
                              ndks123$cat<-NA)))
yhat_123<-matrix(yhat123,resolution,resolution)
#prob123=matrix(c(d1,d2,d3),resolution,resolution)
par(mar=rep(3, 4))

contour(unique(xnew[, 1]), unique(xnew[, 2]), yhat_123, levels = (1:(4-1))+.5, 
        labels=" ", xlab='', ylab='',  lwd=2, lty = 2,
        col='black',ylim=c(-5,8),xlim=c(-5,5), main="BAYES and LDA Decision Boundaries. p=2")

title(xlab=expression(italic('X')[1]), ylab=expression(italic('X')[2]), 
      line=2, family='serif', cex.lab=1.0)
points(xnew, pch=20, cex=0.3, 
       col=ifelse(yhat_123==1 , "red",ifelse(yhat_123==2,"limegreen",ifelse(yhat_123==3,"blue","red"))))
points(df123[,1:2], bg=ifelse(df123[,3]==1, "red", ifelse(df123[,3]==2,"limegreen",ifelse(df123[,3]==3,"blue","transparent"))), pch=21)


ellipse(mu=c(1,5.5), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="blue",lwd=2)

ellipse(mu=c(2,3), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="limegreen",lwd=2)

ellipse(mu=c(-2,2), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="red",lwd=2)
legend("bottomright",legend=c("BAYES DB","LDA DB"),lty=c(2,1),
       pch=c(19,19,19),cex=0.8)

box()

#######

#Bayes error rate and LDA error rate. ISLR page 141

library(ISLR)
library(MASS)
library(wesanderson)
library(shape)
library(nnet)
library( mixtools )
library(caret)

set.seed(12345)
n_k=500
rho=0.75
mu_1=c(-2, 2)
mu_2=c(2, 3)
mu_3=c(1, 5.5)
Sigma=matrix(c(1,rho,rho,1),2,2)

m1<-mvrnorm(n = n_k, mu=mu_1, Sigma=Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
x1=m1[,1]
x2=m1[,2]
y<-rep(1,n_k)
m1=cbind(x1,x2,y)

m2<-mvrnorm(n = n_k, mu=mu_2, Sigma=Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
x1=m2[,1]
x2=m2[,2]
y<-rep(2,n_k)
m2=cbind(x1,x2,y)

m3<-mvrnorm(n = n_k, mu=mu_3, Sigma=Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
x1=m3[,1]
x2=m3[,2]
y<-rep(3,n_k)
m3=cbind(x1,x2,y)

df<-data.frame(rbind(m1,m2,m3))

d_1<-function(x1,x2){matrix(c(x1,x2),1,2)%*%
    ginv(Sigma)%*%matrix(mu_1,2,1)-
    0.5*matrix(mu_1,1,2)%*%ginv(Sigma)%*%matrix(mu_1,2,1)+log(1/3)
}

d_2<-function(x1,x2){matrix(c(x1,x2),1,2)%*%
    ginv(Sigma)%*%matrix(mu_2,2,1)-
    0.5*matrix(mu_2,1,2)%*%ginv(Sigma)%*%matrix(mu_2,2,1)+log(1/3)
}

d_3<-function(x1,x2){matrix(c(x1,x2),1,2)%*%
    ginv(Sigma)%*%matrix(mu_3,2,1)-
    0.5*matrix(mu_3,1,2)%*%ginv(Sigma)%*%matrix(mu_3,2,1)+log(1/3)
}

## 60% of the sample size

smp_size <- floor(0.6 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
train<-train[,1:3]
test <- df[-train_ind, ]
test<-data.frame(test[,1:3])

######BAYES

d1<-rep(NA,nrow(test))
for(i in 1:nrow(test)){
  d1[i]<-d_1(test[i,1],test[i,2])
}

d2<-rep(NA,nrow(test))
for(i in 1:nrow(test)){
  d2[i]<-d_2(test[i,1],test[i,2])
}

d3<-rep(NA,nrow(test))
for(i in 1:nrow(test)){
  d3[i]<-d_3(test[i,1],test[i,2])
}

test1<-data.frame(test,d1,d2,d3)
maximos<-apply(test1[,4:6],1,max)
newtest<-data.frame(test1,maximos)

bayes_class<-ifelse(newtest$maximos==newtest$d1,1,
                    ifelse(newtest$maximos==newtest$d2,2,
                           ifelse(newtest$maximos==newtest$d3,3,NA)))
t<-ifelse(test$y==bayes_class,0,1)
Bayes_err<-sum(t)/length(t)

################LDA

model <- lda(y ~., data=train)
test_yhats<-predict(model,test,type=c("class"))$class
test_lda<-ifelse(test$y==test_yhats,0,1)
LDA_test_err<-sum(test_lda)/nrow(test)

Bayes<-table(test$y,bayes_class)
Lda<-table(test$y,test_yhats)

list(Bayes=Bayes,Bayes_err=Bayes_err,Lda=Lda,LDA_test_err=LDA_test_err)


#######

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

######

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

##########

#Obtaining Fig. 4.7 ISLR. Based on Default dataset
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

threshold<-seq(0.0001,0.6,length.out=50)
LDA_err_YES<-rep(NA,length(threshold))
LDA_err_NO<-rep(NA,length(threshold))
overall_err<-rep(NA,length(threshold))

for(i in 1:length(threshold)){
  df$lda.class<-ifelse(posterior>threshold[i],1,0)
  #Default=Yes
  df2<-df[df$Default==1,]
  train_lda_YES<-ifelse(df2$Default==df2$lda.class,0,1)
  LDA_err_YES[i]<-sum(train_lda_YES)/nrow(df2)
  #Default=No
  df1<-df[df$Default==0,]
  train_lda_NO<-ifelse(df1$Default==df1$lda.class,0,1)
  LDA_err_NO[i]<-sum(train_lda_NO)/nrow(df1)
  #Overall
  train_lda<-ifelse(df$Default==df$lda.class,0,1)
  overall_err[i]<-sum(train_lda)/nrow(df)
}

plot(threshold,LDA_err_NO,col="orange",pch=19,cex=0.7,ylab="Error Rate",xlab="Threshold",ylim=c(0,1),asp=1/3)
lines(threshold,overall_err,lwd=2)
lines(threshold,LDA_err_YES,col="blue",lty=2,lwd=4)
legend(0.05,1,legend=c("Pr(+|-)","Overall Error","Pr(-|+)"),col=c("orange","black","blue"),lty=c(2,1,2),cex=0.7)

######

#5) Assesing the performance of the classifier: Curva ROC
library(ISLR)
library(MASS)
library(Amelia)
library(pscl)
library(caret)
library(pROC)
library(ROCR)
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
lda.class<-ifelse(posterior>0.2,1,0)

Confussion_Matrix<-table(Pred_Default=lda.class,True_Default=df$Default)
train_lda<-ifelse(df$Default==lda.class,0,1)
LDA_train_err<-sum(train_lda)/nrow(df)
list(Confussion_Matrix=Confussion_Matrix,LDA_train_err=LDA_train_err)
#AUC

p <- predict(lda.fit, df[,2:4],type="posterior")$posterior[,2]
pr <- prediction(p, as.factor(df$Default))
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
list(AUC=auc)
p <- predict(lda.fit, df[,2:4],type="posterior")$posterior[,1]
plot(roc(df$Default,1-p,direction="<"),
     col="purple2", lwd=3, main="ROC curve",xaxt="n")

axis(1, at=c(1.5,1,0.5,0,-0.5), labels=c(" ","0.0","0.5","1.0"," "),pos=c(-0.043,-0.043),col="black")
