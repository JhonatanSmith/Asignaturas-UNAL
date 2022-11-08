#########ALGUNOS PROGRAMAS CLASE 8 ISLR#############

library(ISLR)
library(MASS)
Credit<-read.csv(file="Credit.csv",header=T,sep=',',dec='.')
Balance=Credit[,13]
Pred_Cuali<-Credit[,c(9,10,11,12)]
Ethnicity=as.factor(Pred_Cuali[,4])
fit=summary(lm(Balance~Ethnicity))$coefficients
fit

####################################################

library(ISLR)
library(MASS)
library(dummies)
library(olsrr)#Require for selection of variables procedures
Credit<-read.csv(file="Credit.csv", stringsAsFactors=TRUE,
                 header=T,sep=',',dec='.')
Credit1=Credit[,3:13]
#names(ols_step_backward_aic(lm(Balance~., data=Credit1)))
model<-ols_step_backward_aic(lm(Balance~., data=Credit1))
model$model

####################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
fit <- lm(Sales ~ TV + Radio+TV*Radio)
list(Estimates=summary(fit)$coefficients,R2=summary(fit)$r.squared)
z=Advertising$sales
y=Advertising$TV
x=Advertising$radio
fit <- lm(z ~ x + y+x*y)
grid.lines = 50
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
#scatter plot with regression plane with few colors
scatter3D(x, y, z, pch = 19, cex = 1,col=c('red'),
          theta = 25, phi = 5, scale=TRUE, expand=0.3,
          xlab = "Radio", ylab = "Tv", zlab = "Sales",
          surf = list(x = x.pred, y = y.pred, z = z.pred,col='#006633',
                      facets = NA, fit = fitpoints), 
          main = "Advertising Dataset ISLR with interaction")

####################################################

library(ISLR)
library(MASS)
Credit<-read.csv(file="Credit.csv",header=T,sep=',',dec='.')
Balance=Credit[,13]
Income=Credit[,3]
Student=Credit[,10]
modelo1=summary(lm(Balance~Income+Student))$coefficients
modelo2=summary(lm(Balance~Income+Student+Income*Student))$coefficients
list(modelo1=modelo1,modelo2=modelo2)

####################################################

library(ISLR)
library(MASS)
library(ggplot2)
Auto=ISLR::Auto
mpg<-Auto$mpg
horsepower<-Auto$horsepower
fit.lm<-lm(mpg~horsepower,data=Auto)
fit.lm2<-lm(mpg~poly(horsepower,2),data=Auto)
fit.lm5<-lm(mpg~poly(horsepower,5),data=Auto)
d<-seq(40,300,length.out=length(horsepower))
plot(horsepower,mpg,ylim=c(10,50),xlim=c(45,230),col="grey",
     main='Non linear trend between mpg and horsepower. Auto dataset ISLR')
lines(d,predict(fit.lm,data.frame(horsepower=d)),col="orange",lwd=2)
lines(d,predict(fit.lm2,data.frame(horsepower=d)),col="blue",lwd=2)
lines(d,predict(fit.lm5,data.frame(horsepower=d)),col="green",lwd=2)
legend("topright",c("Linear","Degree 2","Degree 5"),
       col=c("orange","blue","green"),lwd=2,cex=0.7)

####################################################

library(ISLR)
library(MASS)
library(ggplot2)
#Auto<-read.csv(file="K:/INTRODUCCIÓN A LA ANALÍTICA/02-2019/Credit.csv",
#                 header=T,sep=',',dec='.')
Auto=ISLR::Auto
mpg<-Auto$mpg
horsepower<-Auto$horsepower
fit.lm<-lm(mpg~horsepower,data=Auto)
fit.lm2<-lm(mpg~poly(horsepower,2),data=Auto)
fit.lm5<-lm(mpg~poly(horsepower,5),data=Auto)
list(degree1=summary(fit.lm)$r.squared,degree2=summary(fit.lm2)$r.squared,
     degree5=summary(fit.lm5)$r.squared)

####################################################

library(MASS)
library("ggplot2")
library(car)
set.seed(5)
N  = 40
b0 = 1.8
b1 = 2
sigma=0.92
x  = seq(-2,4,length.out = N)
y = b0 + b1*x + rnorm(N, mean=0, sd=sigma)
lm.fit=lm(y~x)
outlier=data.frame(x=c(0.3,3.5),y=c(7,14))
newdata=data.frame(y,x)
newdata1=rbind(newdata,outlier)
lm.fit1=lm(y~x,newdata1)
d<-seq(-2,4,length.out = N)
par(mfrow=c(1,3))
plot(x,y,pch=1,cex=1.1,ylab="Y",xlab="X",ylim = c(-4,15),xlim=c(-2,4))
points(x=c(0.3,3.5),y=c(7,14),col=c("orange","purple"),cex=1.3,pch=19)
#Without both the outlier and the high leverage point
lines(d,predict(lm.fit,data.frame(fitted.value=d)),col="blue",lwd=2,lty=2)
#Including both the outlier and the high leverage point
lines(d,predict(lm.fit1,data.frame(fitted.value=d)),col="red",lwd=2)
legend("bottomright",legend=c("Without Leverage","With Leverage"),
       col=c("blue","red"),lty=c(2,1),cex=0.6)

dataEllipse(x, y, levels=c(0.975),ylim=c(-7,15),xlim=c(-4,6),
            lty=2,grid=FALSE,center.pch=1,center.cex=0,xlab="X",ylab="Y",
            col=c("black","limegreen"),lwd=1)
points(x=c(0.3,3.5),y=c(7,14),col=c("orange","purple"),cex=1.3,pch=19)

leverage <- hatvalues(lm(y ~ x, newdata1))
studres<-studres(lm.fit1)
fitted<-lm.fit1$fitted.values

plot(leverage,studres,pch=1,cex=1.1,ylim=c(-1,5),xlim=c(0,0.25),
     ylab="Studentized Residuals",xlab="Leverage")
points(x=c( 0.02796440,0.06926729),y=c(3.52165422,3.84071451),
       col=c("orange","purple"),cex=1.3,pch=19)
abline(h=0,col="purple")

####################################################

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
par(mfrow=c(1,1))
dataEllipse(x, y, levels=c(0.975),ylim=c(-3.3,3.3),
            xlim=c(-3.3,3.3),lty=2,grid=FALSE,center.pch=1,center.cex=0,
            xlab="X1",ylab="X2",col=c("black","blue"),lwd=1)
points(x=c(1,-2.5),y=c(-2,2),col=c("orange","orange"),cex=1.3,pch=19)
