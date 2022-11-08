library(e1071)
names <- read.csv("basesclase4iaa/names.csv",
                  stringsAsFactors=TRUE,sep=';')
names
names_classifier <- naiveBayes(SEX~NAME, data=names)
summary(names_classifier)
test_data=data.frame(NAME=c("MORGAN"))
names_predict <- predict(names_classifier, test_data, type="raw")
names_predict 
sex_predict <- predict(names_classifier, test_data, type="class")
sex_predict 

#########################################

library(e1071)
defaulted <- read.csv("basesclase4iaa/Defaulted_1.csv", 
                      stringsAsFactors=FALSE,sep=';')
train=data.frame(defaulted[1:10,])
test=data.frame(defaulted[11:11,1:4])

defaulted_classifier <- naiveBayes(Defaulted~Home_Owner
                                   +marital_Status+Job_Experience, data=train, laplace=0.128)
defaulted_predict <- predict(defaulted_classifier, test,type="raw")
defaulted_predict

#########################################


library(naivebayes)
set.seed(123)
defaulted <- read.csv("basesclase4iaa/Defaulted_2.csv",
                      stringsAsFactors=FALSE,sep=';') 
df=data.frame(defaulted)
smp_size <- floor(0.75 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size) 
train <- df[train_ind, ]
test <- df[-train_ind, ]
y_train=train$Defaulted
y_test=test$Defaulted
defaulted_classifier <- naive_bayes(Defaulted~Home_Owner
                                    +marital_Status+Job_Experience, data=train[,2:5],laplace=0.128) 
predict_train<-predict(defaulted_classifier,newdata=train[,2:4],type="class")
predict_test<-predict(defaulted_classifier,newdata=test[,2:4],type="class") 
t<-table(predict_train,y_train)
t
t1<-table(predict_test,y_test)
t1
Train_error<-(t[1,2]+t[2,1])/(sum(t)) 
Test_error<-(t1[1,2]+t1[2,1])/(sum(t1))
Train_error

########################################################

library(MASS)
library(class)

cocodrile<-read.csv(file="basesclase4iaa/cocodrilos.csv",header=T,dec=',',sep=';')
#Plot the data
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

plot(cocodrile[,2:3],col=Cols(cocodrile[,4]), pch=19, cex=1.5, xlab="X1", ylab="X2",main='Lagartos versus Cocodrilos')
legend("topleft",legend=c("Lagartos","Cocodrilos"),col=c("cyan","red"),pch=c(19,19),cex=0.8)
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}
df=cocodrile
X_train<-normalize(df[,2:3])
y_train<-cocodrile[,4]
fit.knn_train<-knn(train=X_train, test=X_train,cl=y_train, k=12, prob=TRUE)
Test<-normalize(data.frame(Mouth_Size=c(9,1.2,10,1,1.1), Body_Length=c(7.9,3.7,8,2,2.3)))
fit.knn_Test<-knn(train=X_train, test=Test,cl=y_train, k=12, prob=TRUE)
Predicted<-factor(fit.knn_Test)
prob <- attr(fit.knn_Test, "prob")
Cprob<-1-prob
Type=c('1=C','2=L')
Type
Class_Probs<-cbind(Predicted,prob,Cprob)
Class_Probs

#########################################################
library(MASS)
library(class)
library(naivebayes)
cocodrile<-read.csv("basesclase4iaa/cocodrilos_1.csv",
                    header=T,dec=',',sep=';')
df=data.frame(cocodrile)
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}
smp_size <- floor(0.8 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- normalize(df[train_ind,2:3 ])
test <- normalize(df[-train_ind,2:3 ])
y_train=df[train_ind,4]
y_test=df[-train_ind,4]
fit.knn_train<-knn(train=train, test=train,cl=y_train, k=12, prob=TRUE)
fit.knn_Test<-knn(train=train, test=test,cl=y_train, k=12, prob=TRUE)
Predicted_train<-factor(fit.knn_train)
Predicted_test<-factor(fit.knn_Test)
t<-table(Predicted_train,y_train)
t
t1<-table(Predicted_test,y_test)
t1
Train_error<-(t[1,2]+t[2,1])/(sum(t))
Test_error<-(t1[1,2]+t1[2,1])/(sum(t1))
c(Train_error,Test_error)
