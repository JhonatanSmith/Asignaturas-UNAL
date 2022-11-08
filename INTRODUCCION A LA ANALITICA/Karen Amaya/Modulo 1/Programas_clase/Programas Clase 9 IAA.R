################ALGUNOS PROGRAMAS CLASE 9 IAA########################
library(ISLR)
library(ggplot2)
library(MASS)
#library(plot3D)
library(latex2exp)
#Obtaining figures 3.15 left and right panels
Credit=ISLR::Credit
Limit=Credit[,3]
Limit1=(Limit-mean(Limit))
Age=Credit[,6]
Age1=(Age-mean(Age))
Rating=Credit[,4]
Rating1=(Rating-mean(Rating))
Balance=Credit[,12]
beta1 <- seq(-5,1, length=60)
beta2 <- seq(0.15, 0.20, length = 60)
RSS1=matrix(NA,nrow=length(beta1),ncol=length(beta2))

f<-function(x,y){
  for(i in 1:length(beta1)){for(j in 1:length(beta2)){
    RSS1[i,j]<-sum((Balance-(x[i]*Age1+y[j]*Limit1))^2)}};RSS1
}

RSS<-f(beta1,beta2)/10000000
par(mfrow=c(1,2))
#Contour Plot
contour(beta2,beta1,RSS,levels=c(13,13.05,13.09),col=c("blue")        
        ,xlab=TeX('$\\beta_{Limit}$'),ylab=TeX('$\\beta_{Age}$'))
points(x=seq(0.15,0.173,length=30),y=rep(-2.292,30),pch='-',col="black")
points(x=rep(0.173,30),y=seq(-5,-2.292,length=30),pch=':',cex=1,col="black")
points(x=0.173, y=-2.292, pch=19, col="red",cex=1.5)

beta1 <- seq(-1,5, length=60)
beta2 <- seq(-0.35, 0.35, length = 60)  
RSS1=matrix(NA,nrow=length(beta1),ncol=length(beta2))

f<-function(x,y){
  for(i in 1:length(beta1)){for(j in 1:length(beta2)){
    RSS1[i,j]<-sum((Balance-(x[i]*Rating1+y[j]*Limit1))^2)}};RSS1
}

RSS<-f(beta1,beta2)/10000000
#Contour Plot
contour(beta2,beta1,RSS,levels=c(12.9,13,13.1),col=c("blue")        
        ,xlab=TeX('$\\beta_{Limit}$'),ylab=TeX('$\\beta_{Rating}$'))
points(x=seq(-0.39,0.025,length=30),y=rep(2.202,30),pch='-',col="black")
points(x=rep(0.025,30),y=seq(-1,2.202,length=30),pch=':',cex=1,col="black")
points(x=0.025, y=2.202, pch=19, col="red",cex=1.5)

##############################################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
library(FNN)
library(wesanderson)
library(cowplot)

Advertising<-read.csv(file="../Programas_clase/basesclase8/Advertising.csv",header=T,sep=',',dec='.')
z=Advertising$sales
y=Advertising$TV
x=Advertising$radio
y=(y-mean(y))/sd(y)
x=(x-mean(x))/sd(x)

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

#Like Right Panel
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

#Like Left Panel

fit.knn<-knn.reg(train[,2:3], test = xy, z, k = 12, algorithm=c("cover_tree"))
fit1 <- knn.reg(train[,2:3],test=train[,2:3],z,k=12, algorithm=c("cover_tree"))
fitpoints<-fit1$pred
z.pred <- matrix(fit.knn$pred,nrow = grid.lines, ncol = grid.lines)

scatter3D(x, y, z, pch = 19, cex = 0.8,col=c('purple2'),
          theta = 25, phi = 15, scale=TRUE, expand=0.3,
          xlab = "Radio", ylab = "Tv", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col=heat.colors(5),
                      fit = fitpoints), main = "Knn Regression. k=12")

##############################################################################

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
#1=(x-mean(x))/sd(x)
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


##############################################################################

library(ISLR)
library(MASS)
library(FNN)
library(ggplot2)
library(wesanderson)

Defaulted=ISLR::Default
yes=Defaulted[Defaulted$default=="Yes",]
no1=Defaulted[Defaulted$default== "No",]
set.seed(123)
mult=10
no=no1[sample(nrow(no1), nrow(yes)*mult), ]
Defaulted=rbind(yes,no)
Balance=Defaulted$balance
Income=Defaulted$income
Default=Defaulted$default

Cols=function(vec){
  cols=c("#3399ff","#ff6600")
  return(cols[as.numeric(as.factor(vec))])
}

Pchs=function(vec){
  pchs=1:length(vec)
  return(pchs[as.numeric(as.factor(vec))])
}

layout(matrix(c(rep(1, 10), rep(2, 4), 3, 0, 0, 3), nrow=1, byrow = T))
plot(Balance, Income, col=Cols(Default),pch=Pchs(Default), cex=0.8, xlab="Balance", ylab="Income")


boxplot(Balance~Default,col=c("#3399ff","#ff6600"),xlab="Default",ylab="Balance")
boxplot(Income~Default, col=c("#3399ff","#ff6600"),xlab="Default",ylab="Income")

