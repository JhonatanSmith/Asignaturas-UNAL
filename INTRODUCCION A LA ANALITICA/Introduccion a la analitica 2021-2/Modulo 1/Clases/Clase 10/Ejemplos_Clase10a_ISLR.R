#Ejemplos Clase 10 a ISLR. Clase complementaria

###EJEMPLO 1

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

#**1) The data cleaning process**.
missmap(training.data.raw, main = "Missing values vs observed")

data <- subset(training.data.raw,select=c(1,2,4,5,6,7,9,11))
data$fare<-as.numeric(paste(data$fare))
data$fare[is.na(data$fare)] <- mean(data$fare,na.rm=T)
data$age[is.na(data$age)] <- mean(data$age,na.rm=T)
data <- data[!is.na(data$embarked),]
rownames(data) <- NULL

#**2) Descriptive analysis (in case de number of features is not that big)
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

#**4) Assessing the predictive ability of the model**.
anova(model, test="Chisq")

#While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index 
#can be used to assess the model fit.

pR2(model)

#4) Assessing the predictive ability of the model
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')

fitted.results <- as.factor(ifelse(fitted.results > 0.5,1,0))

misClasificError <- mean(fitted.results != test$survived)
print(paste('Accuracy',1-misClasificError))

#**5) Confussion Matrix**.
confusionMatrix(data=fitted.results, reference=as.factor(test$survived))

#**6) Assesing the performance of the classifier: Curva ROC**.

par(mfrow=c(1,1))
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
plot(roc(test$survived, p, direction="<"),
     col="purple2", lwd=3, main="ROC curve")
#Area under ROC curve (ROC statistic)
pr <- prediction(p, test$survived)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
list(AUC=auc)

#####EJEMPLO 2

#**Ejemplo**: Bayes db, LDA db y LR db:
#CODE TO SIMULATE A FIGURE SIMILAR TO 4.6 ISLR BUT IT IS BASED ON 
#BOTH BAYES CLASSIFIER, LDA AND LR for p=2

library(MASS)
library(class)
library( mixtools )
library(caret)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

set.seed(1234)
n_k=20
rho=0.75
Sigma=matrix(c(1,rho,rho,1),2,2)
mu_1=c(-2 , 2)
mu_2=c(2,3)
mu_3=c(1,5.5)

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
par(mfrow=c(1,1))

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

####################BAYES DECISION BOUNDARIES###################
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

decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, line_color,line_type, ...) 
{
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  #plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  #plot(data, col = c("orange","blue","limegreen"), pch = 20, ...)
  r <- sapply(data, range, na.rm = TRUE)
  # xs <- seq(r[1,1], r[2,1], length.out = resolution)
  # ys <- seq(r[1,2], r[2,2], length.out = resolution)
  xs <- seq(-5, 5, length.out = resolution)
  ys <- seq(-5, 8, length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 3.5, levels = (1:(k-1))+.5,col=line_color,lty=line_type)
  invisible(z)
}

par(mar=rep(3, 4))
contour(unique(xnew[, 1]), unique(xnew[, 2]), yhat_123, levels = (1:(4-1))+.5, 
        labels=" ", xlab='', ylab='',  lwd=2, lty = 2,
        col='black',ylim=c(-5,8),xlim=c(-5,5), 
        main="BAYES, LDA, AND LR Decision Boundaries. p=2")
title(xlab=expression(italic('X')[1]), ylab=expression(italic('X')[2]), 
      line=2, family='serif', cex.lab=1.0)
points(xnew, pch=20, cex=0.3, 
       col=ifelse(yhat_123==1 , "red",ifelse(yhat_123==2,"limegreen",
                                             ifelse(yhat_123==3,"blue","red"))))
points(df123[,1:2], bg=ifelse(df123[,3]==1, "red", ifelse(df123[,3]==2,"limegreen",ifelse(df123[,3]==3,"blue","transparent"))), pch=21)
ellipse(mu=c(1,5.5), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="blue",lwd=2)
ellipse(mu=c(2,3), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="limegreen",lwd=2)
ellipse(mu=c(-2,2), sigma=matrix(c(1,rho,rho,1),2,2), alpha = .05, 
        npoints = 250, newplot = FALSE, draw = TRUE,col="red",lwd=2)
legend("bottomright",legend=c("BAYES DB","LDA DB","LR DB"),lty=c(2,1,1),
       col=c("black","purple2","forestgreen"),pch=c(19,19,19),cex=0.8)
box()

####################LDA DECISION BOUNDARIES###################
#LDA MODEL
model <- lda(y ~ ., data=df)
decisionplot(model, df, class = "y", main = "LDA",line_color="purple2",line_type=1)

####################LR DECISION BOUNDARIES###################
#MULTINOMIAL LOGISTIC REGRESSION MODEL LR

model <- multinom(y ~., data = df,trace=FALSE)
decisionplot(model, df, class = "y", main = "Logistic Regression",line_color="forestgreen",line_type=1)
