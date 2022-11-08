#Ejemplos clase 9 ISLR

####

library(MASS)
library("ggplot2")
library(car)
set.seed(5)
N  = 250
b0 = 0
b1 = 1
sigma=0.25
x  = seq(-2.1,2.1,length.out = N)
y = b0 + b1*x + rnorm(N, mean=0, sd=sigma)
#lm.fit=lm(y~x)
outlier=data.frame(x=c(1,-2.5),y=c(-2,2))
newdata=data.frame(y,x)
newdata1=rbind(newdata,outlier)
lm.fit1=lm(y~x,newdata1)

dataEllipse(x, y, levels=c(0.975),ylim=c(-3.3,3.3),xlim=c(-3.3,3.3),lty=2,grid=FALSE,center.pch=1,center.cex=0,xlab="X1",ylab="X2",col=c("black","blue"),lwd=1)
points(x=c(1,-2.5),y=c(-2,2),col=c("orange","orange"),cex=1.3,pch=19)

#####Knn Regression####

library(ISLR)
library(MASS) 
library("plot3D")
library(FNN)
library(wesanderson)
nuevodata=read.csv(file="Data_Fig_3_16.csv",header=T,sep=',',dec='.')
nuevodata=nuevodata[,2:4]
z=nuevodata$z
x=nuevodata$x
x=(x-mean(x))/sd(x)
y=nuevodata$y
y=(y-mean(y))/sd(y)

par(mfrow=c(1,2))

#Obtaining the surface based on linear regression model (z = ax + by + d)
fit <- lm(z ~ x + y)
# Predict values on regular xy grid
grid.lines = 50
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)

#######Obtaining plots similar to those in Fig 3.16 based on Knn Regression 

df<-data.frame(z,x,y)
## 100% of the sample size
smp_size <- floor(1 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

grid.lines = 85
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
fit.knn<-knn.reg(train[,2:3], test = xy, z, k = 1, algorithm=c("brute"))
fit1 <- knn.reg(train[,2:3],test=train[,2:3], z, k=1, algorithm=c("brute"))
fitpoints<-fit1$pred
z.pred <- matrix(fit.knn$pred,nrow = grid.lines, ncol = grid.lines)

#######Knn with K=1

scatter3D(x, y, z, pch = 19, cex = 0.7,col=c('purple2'),
          theta = 80, phi = 35, expand=0.3,
          xlab = 'X1', ylab = 'X2', zlab = "Y",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col="aquamarine2",
                      facets=NA,fit = fitpoints), main = "Knn Regression K=1.")

#######Knn with K=9

fit.knn<-knn.reg(train[,2:3], test = xy, z, k = 7, algorithm=c("brute"))
fit1 <- knn.reg(train[,2:3], test=train[,2:3], z, k=7, algorithm=c("brute"))
fitpoints<-fit1$pred
z.pred <- matrix(fit.knn$pred,nrow = grid.lines, ncol = grid.lines)

scatter3D(x, y, z, pch = 19, cex = 0.7,col=c('purple2'),
          theta = 80, phi = 35, scale=TRUE, expand=0.3,
          xlab = 'X1', ylab = 'X2', zlab = "Y",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col="aquamarine2",
                      facets=NA,fit = fitpoints), main = "Knn Regression K=7.")

###More on Knn Regression####

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
library(FNN)
library(wesanderson)
library(cowplot)

Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
z=Advertising$sales
y=Advertising$TV
x=Advertising$radio
y=(y-mean(y))/sd(y)
x=(x-mean(x))/sd(x)

#########
#Obtaining the surface based on Knn


df<-data.frame(z,x,y)
## 100% of the sample size
smp_size <- floor(1 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

grid.lines = 80
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)

#######################Like Right Panel
par(mfrow=c(1,2))

fit.knn<-knn.reg(train[,2:3], test = xy, z, k = 3, algorithm=c("cover_tree"))
fit1 <- knn.reg(train[,2:3],test=train[,2:3],z,k=3, algorithm=c("cover_tree"))
fitpoints<-fit1$pred
z.pred <- matrix(fit.knn$pred,nrow = grid.lines, ncol = grid.lines)

scatter3D(x, y, z, pch = 19, cex = 0.8,col=c('purple2'),
          theta = 25, phi = 15, scale=TRUE, expand=0.3,
          xlab = "Radio", ylab = "Tv", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col=heat.colors(5),
                      fit = fitpoints), main = "Knn Regression. k=3")

#######################Like Left Panel

fit.knn<-knn.reg(train[,2:3], test = xy, z, k = 12, algorithm=c("cover_tree"))
fit1 <- knn.reg(train[,2:3],test=train[,2:3],z,k=12, algorithm=c("cover_tree"))
fitpoints<-fit1$pred
z.pred <- matrix(fit.knn$pred,nrow = grid.lines, ncol = grid.lines)

scatter3D(x, y, z, pch = 19, cex = 0.8,col=c('purple2'),
          theta = 25, phi = 15, scale=TRUE, expand=0.3,
          xlab = "Radio", ylab = "Tv", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col=heat.colors(5),
                      fit = fitpoints), main = "Knn Regression. k=12")


############

library(ISLR)
library(MASS)
library(FNN)
library(ggplot2)
library("plot3D")
library(wesanderson)
set.seed(123)
x<-seq(-1,1,length.out=100)
y<-seq(0,4,length.out=100)

y<-2*x
y1<-2*x+rnorm(100,0,0.3)
train=data.frame(x)
fit.knn<-knn.reg(train=x, test = train, y1, k = 1, algorithm=c("brute"))
ypred_Knn<-fit.knn$pred

par(mfrow=c(1,2))


plot(x,y,type='l',col="black",lwd=2,cex=0,main="Knn regression with K=1")
lines(x,ypred_Knn,col="blue",,type="S",lwd=1.5)
points(x,y1,pch=1,cex=0.7,col="red")

#######

fit.knn1<-knn.reg(train=x, test = train, y1, k = 10, algorithm=c("brute"))
ypred_Knn1<-fit.knn1$pred

plot(x,y,type='l',col="black",ylim=c(-2.5,2.5),lwd=2,cex=0,main="Knn regression with K=10")
lines(x,ypred_Knn1,col="blue",,type="S",lwd=1.5)
points(x,y1,pch=1,cex=0.7,col="red")

fit.lm<-lm(y1~x)
ypred_lm<-fit.lm$fitted.value
MSE_lm<-(summary(fit.lm)$sigma)^2

par(mfrow=c(1,2))

plot(x,y,type='l',col="black",lwd=2,cex=0,main="Knn regression with K=10")
lines(x,ypred_lm,col="purple2",type="S",lwd=1.5,lty=2)
points(x,y1,pch=1,cex=0.7,col="red")
lines(x,ypred_Knn1,col="limegreen",type="S",lwd=1.5)
legend("topleft",legend=c("True","OLS","Knn"),col=c("black","purple2","limegreen"),lty=c(1,2),cex=0.3)
#######
K<-sort(seq(1,10,1))
M<-length(K)
train.err<-rep(NA,M)
flex_knn<-rep(NA,M)
train=data.frame(x)

for (i in 1:M){
  fit.knn_train<-knn.reg(train=x, test=train,y1, k=K[i])
  train.err[i]<-mean((y1-fit.knn_train$pred ) ^ 2)
  flex_knn[i]<-1/K[i]  
}
df11<-data.frame(log(flex_knn),log(train.err+0.8))
plot(df11[,1],-df11[,2],col=c('forestgreen'), pch=1,type='b',lty=4,cex = 0.7,xlab='1/K',ylab='Mean Squared Error',
     main='Mean squared Error \n as function of 1/K',xaxt = "n",ylim=c(0,0.25),lwd=2)
abline(h=MSE_lm,col="purple2",lwd=1,lty=2)
axis(1, at=df11[,1], labels=exp(df11[,1]))
legend("topleft",legend=c("Knn MSE","LRM MSE"),col=c("limegreen","purple2"),lty=c(1,2),cex=0.3,lwd=c(2,1))

dev.off()
#####

library(ISLR)
library(MASS)
library(FNN)
library(ggplot2)
library("plot3D")
library(wesanderson)
set.seed(123)
x<-seq(-1,1,length.out=100)
y<- 9/5+ (5.9850/5)*x+(0.2676/5)*x^2-(2.0849/5)*x^3
y1<-y+rnorm(100,0,0.15)

### Fitting linear model

lm.fit<-lm(y1~x)
MSE_lm<-summary(lm.fit)$sigma

###Panel(1,1) Figure 3.19
train=data.frame(x)
fit.knn<-knn.reg(train=x, test = train, y1, k = 1, algorithm=c("brute"))
ypred_Knn<-fit.knn$pred

fit.knn1<-knn.reg(train=x, test = train, y1, k = 10, algorithm=c("brute"))
ypred_Knn1<-fit.knn1$pred

plot(x,y,type='l',col="black",lwd=2,cex=0,main="Knn regression",ylab="Y",xlab="X",ylim=c(0.5,3.5))
lines(x,ypred_Knn,col="blue",,type="S",lwd=1.5)
lines(x,ypred_Knn1,col="red",,type="S",lwd=1.5)
points(x,y1,pch=1,cex=0.0,col="red")
legend("topleft",legend=c("True","Knn K=1","Knn K=10"),col=c("black", "blue","red"),lty=c(1,2),cex=0.4,lwd=c(2,1))

###Panel(1,2)

K<-sort(seq(1,10,1))
M<-length(K)
train.err<-rep(NA,M)
flex_knn<-rep(NA,M)
train=data.frame(x)

for (i in 1:M){
  fit.knn_train<-knn.reg(train=x, test=train,y1, k=K[i])
  train.err[i]<-mean((y1-fit.knn_train$pred ) ^ 2)
  flex_knn[i]<-1/K[i]  
}
df11<-data.frame(log(flex_knn),log(train.err+0.846))
plot(df11[,1],-df11[,2],col=c('forestgreen'), pch=1,type='b',lty=4,cex = 0.7,xlab='1/K',ylab='Mean Squared Error',
     main='MSE as function of 1/K',ylim=c(0,0.24),xaxt = "n",lwd=2)
abline(h=MSE_lm,col="purple2",lwd=1,lty=2)
axis(1, at=df11[,1], labels=exp(df11[,1]))
legend("bottomleft",legend=c("Knn MSE","LRM MSE"),col=c("forestgreen","purple2"),lty=c(1,2),cex=0.6,lwd=c(2,1))

####Same as before but with a highly nonlinear function

x<-seq(-1,1,length.out=100)
y<- 5.0145+ 3.9850*x-0.02676*x^2-3.0849*x^3
y<-y/2 #Esta es la del panel(2,1)
y1<-y+rnorm(100,0,0.15)

### Fitting linear model

lm.fit<-lm(y1~x)
MSE_lm<-summary(lm.fit)$sigma

###Panel(2,1)
###Panel(1,1) Figure 3.19
train=data.frame(x)
fit.knn<-knn.reg(train=x, test = train, y1, k = 1, algorithm=c("brute"))
ypred_Knn<-fit.knn$pred

fit.knn1<-knn.reg(train=x, test = train, y1, k = 10, algorithm=c("brute"))
ypred_Knn1<-fit.knn1$pred

plot(x,y,type='l',col="black",lwd=2,cex=0,main="Knn regression",ylab="Y",xlab="X",ylim=c(1.5,3.5))
lines(x,ypred_Knn,col="blue",,type="S",lwd=1.5)
lines(x,ypred_Knn1,col="red",,type="S",lwd=1.5)
points(x,y1,pch=1,cex=0.0,col="red")
legend("topleft",legend=c("True","Knn K=1","Knn K=10"),col=c("black", "blue","red"),lty=c(1,2),cex=0.6,lwd=c(2,1))

###Panel(2,2)

K<-sort(seq(1,10,1))
M<-length(K)
train.err<-rep(NA,M)
flex_knn<-rep(NA,M)
train=data.frame(x)

for (i in 1:M){
  fit.knn_train<-knn.reg(train=x, test=train,y1, k=K[i])
  train.err[i]<-mean((y1-fit.knn_train$pred ) ^ 2)
  flex_knn[i]<-1/K[i]  
}
df11<-data.frame(log(flex_knn),log(train.err+0.846))
plot(df11[,1],-df11[,2],col=c('forestgreen'), pch=1,type='b',lty=4,cex = 0.7,xlab='1/K',ylab='Mean Squared Error',
     main='MSE as function of 1/K',ylim=c(0,0.35),xaxt = "n",lwd=2)
abline(h=MSE_lm,col="purple2",lwd=1,lty=2)
axis(1, at=df11[,1], labels=exp(df11[,1]))
legend("bottomleft",legend=c("Knn MSE","LRM MSE"),col=c("forestgreen","purple2"),lty=c(1,2),cex=0.6,lwd=c(2,1))

