##########ALGUNOS PROGRAMAS CLASE 5 ISLR########################################
library(class)
library(naivebayes)
library(dummies)
set.seed(123)
defaulted <- read.csv("basesclase5iaa/Defaulted_3.csv", 
                      stringsAsFactors=FALSE,sep=';')
#defaulted <- read.csv("Defaulted_3.csv", 
#                      stringsAsFactors=FALSE,sep=';')
df=data.frame(defaulted)
## 100*P% of the sample size
smp_size <- floor(0.6 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]
y_train=df[train_ind,5]
y_test=df[-train_ind,5]
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}
###########Naive Bayes################################
defaulted_classifier <- naive_bayes(Defaulted~Home_Owner
                                    +marital_Status+Job_Experience, 
                                    data=train[,2:5],laplace=0.128)
predict_train<-predict(defaulted_classifier,newdata=train[,2:4],type="class")
predict_test<-predict(defaulted_classifier,newdata=test[,2:4],type="class")
t<-table(predict_train,y_train)
t1<-table(predict_test,y_test)
Train_error_NB<-(t[1,2]+t[2,1])/(sum(t))
Test_error_NB<-(t1[1,2]+t1[2,1])/(sum(t1))
#######Knn########################
dummyHO<-dummy(df$Home_Owner ,sep="_")
dummyMS<-dummy(df$marital_Status, sep="_")
job<-normalize(df$Job_Experience)
Newdata<-cbind(df,dummyHO,dummyMS,job)
train1 <- Newdata[ train_ind,6:11 ]
test1  <- Newdata[-train_ind,6:11 ]
y_train1=df[train_ind,5]
y_test1=df[-train_ind,5]
fit.knn_train<-knn(train=train1, test=train1,cl=y_train1, k=1, prob=TRUE,use.all=TRUE)
fit.knn_Test<-knn(train=train1, test=test1,cl=y_train1, k=1, prob=TRUE)
Predicted_train<-factor(fit.knn_train)
Predicted_test<-factor(fit.knn_Test)
t<-table(Predicted_train,y_train)
t1<-table(Predicted_test,y_test1)
Train_error_Knn<-(sum(t[1,2],t[2,1]))/(sum(t))
Test_error_Knn<-(sum(t1[1,2],t1[2,1]))/(sum(t1))
list(Train_error_NB=Train_error_NB,Test_error_NB=Test_error_NB,
     Train_error_Knn=Train_error_Knn,Test_error_Knn=Test_error_Knn)

##############################################################################

library(ISLR)
library(ggplot2)
library(MASS)
Advertising<-read.csv(file="basesclase5iaa/Advertising.csv"
                      ,header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv"
#                      ,header=T,sep=',',dec='.')

Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
mod <- lm(Sales ~ Tv, data = Advertising)
summary(mod)

Advertising <- transform(Advertising, Fitted = fitted(mod))

ggplot(Advertising, aes(Tv,Sales,color='blue')) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, colour="purple") +
  labs(x='TV', y='Sales')+
  geom_segment(aes(x = Tv, y = Sales,
                   xend = Tv, yend = Fitted))
################################################################################
library(ISLR)
library(ggplot2)
library(MASS)
Advertising<-read.csv(file="basesclase5iaa/Advertising.csv"
                      ,header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv"
#                      ,header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
mod <- lm(Sales ~ Tv, data = Advertising)
summary(mod)
#################################################################################
library(ISLR)
library(ggplot2)
library(MASS)
Advertising<-read.csv(file="basesclase5iaa/Advertising.csv"
                      ,header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv"
#                      ,header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
mod <- lm(Sales ~ Tv, data = Advertising)
Advertising <- transform(Advertising, Fitted = fitted(mod))

ggplot(Advertising, aes(Tv,Sales,color='blue')) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE, colour="purple") +
  labs(x='TV', y='Sales')+
  geom_segment(aes(x = Tv, y = Sales,
                   xend = Tv, yend = Fitted))
#############################################################################
library(ISLR)
library(ggplot2)
library(MASS)
#library(plot3D)
library(latex2exp)
#Obtaining figures 3.1 left and right panels
Advertising<-read.csv(file="basesclase5iaa/Advertising.csv"
                      ,header=T,sep=',',dec='.')
#Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper
RSS2<-sum((Sales-7.03-0.0475*Tv)^2)
beta0 <- seq(4, 10, length = 50)  ## already in increasing order
beta1 <- seq(0.02,0.07, length=50)
RSS1=matrix(NA,nrow=length(beta0),ncol=length(beta1))

f<-function(x,y){
  for(i in 1:length(beta0)){for(j in 1:length(beta1)){
    RSS1[i,j]<-sum((Sales-x[i]-y[j]*Tv)^2)}};RSS1
}

RSS<-f(beta0,beta1)/1000

#Contour Plot
contour(beta0,beta1,RSS,xlim=c(4,10),ylim=c(0.02,0.07),nlevel=17,col=c("blue")
        ,xlab=TeX('$\\beta_0$'),ylab=TeX('$\\beta_1$'))
points(x=7.03, y=0.0475, pch=19, col="red",cex=1.5)
#############################################################################
