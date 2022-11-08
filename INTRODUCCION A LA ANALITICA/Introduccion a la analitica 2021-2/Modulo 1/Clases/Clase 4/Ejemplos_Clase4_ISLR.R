#Ejemplos clase 4 ISLR

library(e1071)
names <- read.csv("names.csv", stringsAsFactors=TRUE,sep=';')
names_classifier <- naiveBayes(SEX~NAME, data=names)
summary(names_classifier)
test_data=data.frame(NAME=c("MORGAN"))
names_predict <- predict(names_classifier, test_data, type="raw")
names_predict

######
defaulted <- read.csv("Defaulted_1.csv", stringsAsFactors=FALSE,sep=';')
train=data.frame(defaulted[1:10,])
test=data.frame(defaulted[11:11,1:4])
defaulted_classifier <- naiveBayes(Defaulted~Home_Owner
                                   +marital_Status+Job_Experience, data=train, laplace=0.128)
defaulted_predict <- predict(defaulted_classifier, test,type="raw")
defaulted_predict

######

#Calculating the test error rate and the training error rate
library(naivebayes)
set.seed(1)
defaulted <- read.csv("Defaulted_2.csv", stringsAsFactors=FALSE,sep=';')
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
t1<-table(predict_test,y_test)
Train_error<-(t[1,2]+t[2,1])/(sum(t))
Test_error<-(t1[1,2]+t1[2,1])/(sum(t1))
Train_error
Test_error