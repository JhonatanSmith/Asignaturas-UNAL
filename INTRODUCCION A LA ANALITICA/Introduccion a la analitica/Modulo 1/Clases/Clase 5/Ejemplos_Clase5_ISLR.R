library(class)
library(naivebayes)
library(dummies)
set.seed(123)
defaulted <- read.csv("Defaulted_3.csv",
                      stringsAsFactors=FALSE,sep=';')
head(defaulted)
#defaulted <- read.csv("Defaulted_3.csv",
# stringsAsFactors=FALSE,sep=';')
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
test1 <- Newdata[-train_ind,6:11 ]
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

