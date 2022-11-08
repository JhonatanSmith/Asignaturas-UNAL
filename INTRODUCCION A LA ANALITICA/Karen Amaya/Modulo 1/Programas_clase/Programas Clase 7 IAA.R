#########ALGUNOS PROGRAMAS CLASE 7 ISLR#############

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit <- lm(Sales ~ TV+Radio+Newspaper)
summary(fit)
fit0<-lm(Sales~1)
anova(fit0,fit)

####################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit <- lm(Sales ~ TV+Radio+Newspaper)
summary(fit)
fit0<-lm(Sales~TV+Radio)
anova(fit0,fit)

####################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit <- lm(Sales ~ TV+Radio+Newspaper)
summary(fit)
fit0<-lm(Sales~TV+Newspaper)
anova(fit0,fit)

####################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit <- lm(Sales ~ TV+Radio+Newspaper)
summary(fit)
fit0<-lm(Sales~Radio+Newspaper)
anova(fit0,fit)

####################################################

library(ISLR)
library(MASS)
library(olsrr)#Require for selection of variables procedures
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Advertising=Advertising[2:5] #To eliminate ID variable

ols_step_backward_aic(lm(sales~., data=Advertising))

ols_step_forward_aic(lm(sales~., data=Advertising))

ols_step_both_aic(lm(sales~., data=Advertising))

####################################################

library(ISLR)
library(MASS)
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit1 <- lm(Sales ~ TV+Radio+Newspaper)
fit2<-lm(Sales~TV+Radio)
list(R2_Modelo1=summary(fit1)$r.squared,R2_Modelo2=summary(fit2)$r.squared)

####################################################

library(ISLR)
library(MASS)
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit1 <- lm(Sales ~ TV)
list(R2_Modelo1=summary(fit1)$r.squared)

####################################################

library(ISLR)
library(MASS)
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit2 <- lm(Sales ~ TV+Radio)
summary(fit2)$sigma
list(R2_Modelo2=summary(fit2)$r.squared)

####################################################

library(ISLR)
library(MASS)
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit3 <- lm(Sales ~ Radio)
list(R2_Modelo3=summary(fit3)$r.squared)

####################################################

library(ISLR)
library(MASS)
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
fit2 <- lm(Sales ~ TV+Radio)
fit3 <- lm(Sales ~ TV+Radio+Newspaper)
fit4 <- lm(Sales ~ TV)
list(RSE_Modelo2=summary(fit2)$sigma,RSE_Modelo3=summary(fit3)$sigma,RSE_Modelo4=summary(fit4)$sigma)

####################################################

library(ISLR)
library(ggplot2)
library(MASS)
library("plot3D")
Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
z=Advertising$sales
y=Advertising$TV
x=Advertising$radio
#Newspaper=Advertising$newspaper
# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
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
                      facets = NA, fit = fitpoints), main = "Advertising Dataset ISLR")


####################################################

library(ISLR)
library(MASS)
Advertising<-read.csv(file="Advertising.csv",
                      header=T,sep=',',dec='.')
Sales=Advertising$sales
TV=Advertising$TV
Radio=Advertising$radio
fit <- lm(Sales ~ TV+Radio)
newdata<-data.frame(TV=100,Radio=20)#Los valores con los que se evaluan los intervalos
CI<-predict(fit,newdata,interval="confidence");length_CI=CI[3]-CI[2]
PI<-predict(fit,newdata,interval="predict");length_PI=PI[3]-PI[2]
list(Confidence_Interval=CI,length_CI=length_CI,Prediction_Interval=PI,length_PI=length_PI)

####################################################

library(ISLR)
library(MASS)
#Credit=ISLR::Credit
Credit<-read.csv(file="Credit.csv",header=T,sep=',',dec='.')
Pred_Cuanti<-Credit[,c(13,7,6,8,3,4,5)]
# head(Credit)
# head(Pred_Credit)
pairs(Pred_Cuanti,pch=1,cex=0.1,col="blue")

####################################################

library(ISLR)
library(MASS)
Credit<-read.csv(file="Credit.csv",header=T,sep=',',dec='.')
Balance=Credit[,13]
Pred_Cuali<-Credit[,c(9,10,11,12)]
Gender=as.factor(Pred_Cuali[,1])
fit=summary(lm(Balance~Gender))$coefficients
fit
