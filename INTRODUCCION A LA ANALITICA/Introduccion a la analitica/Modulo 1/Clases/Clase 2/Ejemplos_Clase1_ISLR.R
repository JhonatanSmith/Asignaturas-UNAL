#Ejemplos clase 2 ISLR
#School os Statistics.
library(ISLR)
library(ggplot2)
library(MASS)

#Loading Wage dataset
Wage=ISLR::Wage
Age=Wage$age
Wage_=Wage$wage

ggplot(Wage, aes(Age,Wage_,color=cyl)) + 
  geom_point(color='green') +
  geom_smooth(method = "loess", se = FALSE, colour="purple")#Yet another way
Year=Wage$year
plot(Year,Wage_,col='gray',ylim=c(50,300),ylab='Wage')
abline(lm(Wage_ ~ Year,data=Wage),col='blue',lwd=3)
Education=Wage$education
boxplot(Wage_~Education,data=Wage, main="Boxplot of Education vs Wage",
        xlab="Education level", ylim=c(50,300),ylab="Wage",col=c(2,3,4,5,6))

#Loading Smarket dataset
Smarket=ISLR::Smarket
Pchange_Yesterday=Smarket$Lag1
Pchange_Twodays=Smarket$Lag2
Pchange_Threedays=Smarket$Lag3
Direction=Smarket$Direction
Year=Smarket$Year
boxplot(Pchange_Yesterday~Direction,data=Smarket, main="Yesterday. S&P500",
        xlab="Today큦 direction", ylim=c(-4,6),ylab="Percentage change in S&P",col=c(4,2))
boxplot(Pchange_Twodays~Direction,data=Smarket, main="Two days ago. S&P500",
        xlab="Today큦 direction", ylim=c(-4,6),ylab="Percentage change in S&P",col=c(4,2))
boxplot(Pchange_Threedays~Direction,data=Smarket, main="Three days ago. S&P500",
        xlab="Today큦 direction", ylim=c(-4,6),ylab="Percentage change in S&P",col=c(4,2))
#Discriminant analysis
train=(Year<2005)
length(train)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
length(Direction.2005)
#Linear discriminant
lda.fit=lda(Direction~Pchange_Yesterday+Pchange_Twodays,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Smarket.2005)$class
length(lda.pred)
table(lda.pred[999:1250],Direction.2005)

#Quadratic discriminant. 
qda.fit=qda(Direction~Pchange_Yesterday+Pchange_Twodays,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)
names(qda.class)
qda.class=predict(qda.fit,Smarket.2005)$class
length(qda.class)
table(qda.class[999:1250],Direction.2005)
summary(qda.class)
qda.classif=qda.class[999:1250]
qda.prob=predict(qda.fit,Smarket.2005)$posterior
qda.posterior=qda.prob[999:1250]
par(mfrow=c(1,1))
boxplot(qda.posterior~Direction.2005,col=c(5,2),ylab='Posterior probability',xlab='Today큦 direction')

#PCA NCI60 data. 
#Data is in COPY1_OF_figure1_4_islr.csv"
par(mfrow=c(1,2))
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
figure1_4_left2<-read.csv(file='COPY1_OF_figure1_4_islr.csv',header=T,sep=',',dec='.')
plot(figure1_4_left2[,1:2],col=Cols(figure1_4_left2[,3]), pch=19, xlab="Z1", ylab="Z2",main='NCI60 in R2')

#PCA NCI60 data. Obtaining figure 1.4 right panel
nci.data=NCI60$data
nci.labs=NCI60$labs
dim(nci.data)
pr.out=prcomp(nci.data, scale=TRUE)
names(pr.out)
dim(pr.out$x)

plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2")

