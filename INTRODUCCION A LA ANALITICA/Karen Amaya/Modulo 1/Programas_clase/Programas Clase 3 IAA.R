library(MASS)
m1<-mvrnorm(n = 50, mu=c(2.5,3), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m1[,1]
X2=m1[,2]
m1=cbind(X1,X2)
m2<-mvrnorm(n = 50, mu=c(2.5,9), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m2[,1]
X2=m2[,2]
m2=cbind(X1,X2)
m3<-mvrnorm(n = 50, mu=c(9,6), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m3[,1]
X2=m3[,2]
m3=cbind(X1,X2)
plot(m1[,1], m1[,2],  col="darkorange", pch=1, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12),main='Three well separated groups') 
points(m2[,1], m2[,2],  col="dodgerblue", pch=2, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12)) 
points(m3[,1], m3[,2],  col="darkgreen", pch=3, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12)) 
legend("bottomright",c("Group 1","Group 2", "Group 3"),col=c("dodgerblue","darkorange","darkgreen"),pch=c(2,1,3),cex=1.1)

############################################################

#Obtaining plots like Figure 2.8. Simulated data.
library(MASS)
#RIGHT PANEL
m1<-mvrnorm(n = 50, mu=c(2.5,7.2), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m1[,1]
X2=m1[,2]
m1=cbind(X1,X2)
m2<-mvrnorm(n = 50, mu=c(2.5,9), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m2[,1]
X2=m2[,2]
m2=cbind(X1,X2)
m3<-mvrnorm(n = 50, mu=c(4.5,6.5), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m3[,1]
X2=m3[,2]
m3=cbind(X1,X2)
plot(m1[,1], m1[,2], col="darkorange", pch=1, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,8),main='Three well mixed  groups. Separation no clear') 
points(m2[,1], m2[,2], col="dodgerblue", pch=2, xlab="X1", ylab="X2") 
points(m3[,1], m3[,2], col="darkgreen", pch=3, xlab="X1", ylab="X2") 
legend("bottomright",c("Group 1","Group 2", "Group 3"),col=c("dodgerblue","darkorange","darkgreen"),
       pch=c(2,1,3),cex=0.9)

########################################################

#Simulated data similar in shape to the one in figure 2.9. Left Panel
set.seed(123)
x<-seq(0,99)
aux<-rep(0,100)
e<-rnorm(aux,0,1.2)
y<--2.8*cos((2*pi/130)*x)+6.8+e
fit.lm<-lm(y~x)
ylr<-fit.lm$fitted.values
#fit.loess<-loess(y~x)
fit.loess<-loess(y~x,span=0.3,degree=1)
fit.loess1<-suppressWarnings(loess(y~x,span=0.08,degree=1))
yloess<-fit.loess$fitted
yloess1<-fit.loess1$fitted
x1 = seq(0,100,.1)
truth<--2.8*cos((2*pi/130)*x1)+6.8
plot(x,y,ylim=c(0,12),xlim=c(0,100),main='True curve and three estimates. Simulated data')
lines(x1,truth,col="red",lwd=2)
lines(x,ylr,col="orange",lwd=2)
lines(x,yloess,col="blue",lwd=2)
lines(x,yloess1,col="green",lwd=2)
legend("bottomright",c("True f(x)","LR","Loess high smooth","Loess low smooth"),col=c("red","orange","blue","green"),lwd=2,cex=0.7)

###########################################################

library(splines)
options(warn=-1)
set.seed(123)
x<-seq(0,99)
aux<-rep(0,100)
e<-rnorm(aux,0,1.3)
y<--2.8*cos((2*pi/130)*x)+6.8+e
df<-data.frame(x,y)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]
fit.lm<-lm(y~x,data=train)
fit.loess<-suppressWarnings(loess(y~x,span=0.3,degree=1,data=train,control=loess.control(surface = "direct")))
fit.loess1<-suppressWarnings(loess(y~x,span=0.08,degree=1,data=train,control=loess.control(surface = "direct")))
flex_mod1<-fit.lm$rank
flex_mod2<-fit.loess$enp
flex_mod3<-fit.loess1$enp
#Training MSE for LR
train_MSE1=mean(fit.lm$residuals^2)
#Training MSE for Loess high smoothing
train_MSE2=mean(fit.loess$residuals^2)
#Training MSE for Loess low smoothing
train_MSE3=mean(fit.loess1$residuals^2)
#Test MSE
test_MSE1<-mean((test$y - predict.lm(fit.lm, test)) ^ 2)
test_MSE2<-mean((test$y - predict(fit.loess, test)) ^ 2)
test_MSE3<-mean((test$y - predict(fit.loess1, test)) ^ 2)
Flexibility<-c(flex_mod1,flex_mod2,flex_mod3)
TrainMSE<-c(train_MSE1,train_MSE2,train_MSE3)
TestMSE<-c(test_MSE1,test_MSE2,test_MSE3)
list(Flexibility=Flexibility,TrainMSE=TrainMSE,TestMSE=TestMSE)

#####################################################################

#let us check the bias variance tradeoff

f = function(x) {
  -0.00021999*(0.88*x-40)^3+12
}

get_sim_data = function(f, sample_size = 100) {
  x=seq(0,99)
  y = f(x) + rnorm(n = sample_size, mean = 0, sd = 1.2)
  data.frame(x, y)
}

#set.seed(1)
n_sims = 2000
n_models = 3
x0 = 60
predictions = matrix(0, nrow = n_sims, ncol = n_models)
#sim_data = get_sim_data(f, sample_size = 100)

for (i in 1:n_sims) {
  
  sim_data = get_sim_data(f, sample_size = 100)
  fit_1<-lm(y~x,data=sim_data)
  fit_2<-loess(y~x,span=0.3,degree=1,data=sim_data,control=loess.control(surface = "direct"))
  fit_3<-suppressWarnings(loess(y~x,span=0.09,degree=1,data=sim_data,control=loess.control(surface = "direct")))
  predictions[i, ] = c(
    predict(fit_1, newdata = data.frame(x = x0)),
    predict(fit_2, newdata = data.frame(x = x0)),
    predict(fit_3, newdata = data.frame(x = x0))
  )
}

e = rnorm(n = n_sims, mean = 0, sd = 1.2)
y0 = f(x0) + e

# we add functions for bias and mean squared error.

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_mse = function(estimate, truth) {
  mean((estimate - truth) ^ 2)
}

#Use the predictions obtained from the above simulation to estimate the bias,
# variance and mean squared error for estimating f(x) at x0=given value for the models.

bias = apply(predictions, 2, get_bias, f(x0))
sqrBias=bias^2
variance = apply(predictions, 2, var)
MSE = apply(predictions, 2, get_mse, y0)

#CHECKING THE BIAS-VARIANCE EQUATION FOR EACH MODEL
c('CHECKING THE BIAS-VARIANCE EQUATION FOR EACH MODEL:')
c('sqrBias + variance + var(e), FOR EACH MODEL:')
sqrBias + variance + var(e)
c('MSE FOR EACH MODEL:')
MSE
c('Approximation is considered a good one!!!!')

