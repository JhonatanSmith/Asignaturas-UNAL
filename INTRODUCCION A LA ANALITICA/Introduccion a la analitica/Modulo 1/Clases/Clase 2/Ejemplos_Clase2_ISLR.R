#Ejemplos clase 2 ISLR
#School os Statistics.
library(ISLR)
library(ggplot2)
library(MASS)
#Obtaining figure 2.1
Advertising<-read.csv(file="Advertising.csv",header=T,sep=',',dec='.')
head(Advertising)
Sales=Advertising$sales
Tv=Advertising$TV
Radio=Advertising$radio
Newspaper=Advertising$newspaper

ggplot(Advertising, aes(Tv,Sales,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, colour="purple")
ggplot(Advertising, aes(Radio,Sales,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, colour="purple")
ggplot(Advertising, aes(Newspaper,Sales,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, colour="purple")
#Obtaining figure 2.2
Income=read.csv(file="Income1.csv",header=T,sep=',',dec='.')
head(Income)
YofEdu=Income$Education
Income_=Income$Income

ggplot(Income, aes(YofEdu,Income_,color='red')) + 
  geom_point(color='red') +
  labs(x='years of Education', y='Income')

ggplot(Income, aes(YofEdu,Income_,color='red')) + 
  geom_point(color='red') +
  geom_smooth(method = "loess", se = FALSE, colour="purple") +
  labs(x='years of Education', y='Income')
 
#With joining line segments

mod <- loess(Income ~ YofEdu, data = Income)
Income <- transform(Income, Fitted = fitted(mod))

  ggplot(Income, aes(YofEdu,Income_,color='red')) + 
    geom_point(color='red') +
    geom_smooth(method = "loess", se = FALSE, colour="purple") +
    labs(x='years of Education', y='Income')+
geom_segment(aes(x = YofEdu, y = Income,
                 xend = YofEdu, yend = Fitted))

#Joining line segments using base plot
  plot(Income ~ YofEdu, data = Income, type = "p", col = "red",
       cex = 1.25,xlab='Years of Education')
  points(Fitted ~ YofEdu, data = Income)
  lines(Fitted ~ YofEdu, data = Income, col = "blue")
  with(Income, segments(YofEdu, Income, YofEdu, Fitted))

  plot(Income ~ YofEdu, data = Income, type = "p", col = "red",pch=19,
       cex = 1,xlab='Years of Education')
  points(Fitted ~ YofEdu, data = Income,pch=19,
         cex = 0.5)
  lines(Fitted ~ YofEdu, data = Income, col = "blue")
  with(Income, segments(YofEdu, Income, YofEdu, Fitted))
  
#Obtaining figure 2.3
Income3D=read.csv(file="Income2.csv",header=T,sep=',',dec='.')
head(Income3D)
x=Income3D$Education
y=Income3D$Seniority
z=Income3D$Income
#dev.off()

xyz.fit <- loess(z~x+y+x*y,span=0.5,degree=2); d1<-0.5; d2<-10
f1<-function(x,y){predict(xyz.fit,cbind(x,y))};
x1<-sort(seq(min(x),max(x),d1)); y1<-sort(seq(min(y),max(y),d2)); z1<-outer(x1,y1,f1)
res<-persp(x=x1,y=y1, z=z1,phi=25,theta=40,shade=0.2,scale=TRUE,col="lightblue", expand=0.4,
           box = TRUE, xlab="years of Education",ylab="Seniority",zlab="Income")
mypoints <- trans3d(x, y, z, pmat=res); points(mypoints, pch=19, col="red")

dev.off()
#Another way
fit.loess<-loess(z~x+y+x*y,span=0.5,degree=2)
xnew <- seq(min(x), max(x), len=30)
ynew <- seq(min(y), max(y), len=30)
df <- expand.grid(x = xnew, y = ynew)

f1<-function(x,y){predict(fit.loess,newdata=df)}
z1<-outer(xnew,ynew,f1)
persp(xnew,ynew,z1,phi=25,theta=40,shade=0.2,scale=TRUE,col="lightblue", expand=0.4,
      box = TRUE, xlab="years of Education",ylab="Seniority",zlab="Income")
mypoints <- trans3d(x, y, z, pmat=res); points(mypoints, pch=19, col="red")


# Yet another way using plotly
require(plotly)
library(plotly)
aX <- list(title = "years of Education")
aY <- list(title = "Seniority")
aZ <- list(title = "Income")
df2=data.frame(x,y,z)
df2=t(df2)
z1=t(z1)
plot_ly() %>% add_surface(x=xnew,y=ynew,z=z1)%>%
 layout(scene = list(xaxis = aX, yaxis = aY, zaxis=aZ,dragmode="turntable"),autosize=0.9)%>% 
  add_trace(data = df2, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))

plot_ly(x=xnew,y=ynew,z=z1,type = "surface",colors=colors) %>%
  layout(scene = list(xaxis = aX, yaxis = aY,  zaxis=aZ, dragmode="turntable"))%>% 
  add_trace(data = df2, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))  

#Figure 2.4. Fitting a plane
fit.lm<-lm(z~x+y)
summary(fit.lm)
xnew <- seq(min(x), max(x), len=30)
ynew <- seq(min(y), max(y), len=30)
df <- expand.grid(x = xnew, y = ynew)
dim(df)
f1<-function(x,y){predict(fit.lm,newdata=df)}
z2<-outer(xnew,ynew,f1)
persp(xnew,ynew,z2,phi=25,theta=40,shade=0.2,scale=TRUE,col="yellow", expand=0.4,
      box = TRUE, xlab="years of Education",ylab="Seniority",zlab="Income")
mypoints <- trans3d(x, y, z, pmat=res); points(mypoints, pch=19, col="red")

# Yet another way using plotly
require(plotly)
library(plotly)
aX <- list(title = "years of Education")
aY <- list(title = "Seniority")
aZ <- list(title = "Income")
df3=data.frame(x,y,z)
head(df3)
df3=t(df3)
z2=t(z2)
plot_ly() %>% add_surface(x=xnew,y=ynew,z=z2)%>%
  layout(scene = list(xaxis = aX, yaxis = aY, zaxis=aZ,dragmode="turntable"),autosize=1.9)%>% 
  add_trace(data = df3, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))

plot_ly(x=xnew,y=ynew,z=z2,type = "surface",colors=colors) %>%
  layout(scene = list(xaxis = aX, yaxis = aY,  zaxis=aZ, dragmode="turntable"))%>% 
  add_trace(data = df3, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))  


#Obtaining plots like Figure 2.8
#Left Panel
m1<-mvrnorm(n = 50, mu=c(2.5,3), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
head(m1)
X1=m1[,1]
X2=m1[,2]
m1=cbind(X1,X2)

m2<-mvrnorm(n = 50, mu=c(2.5,9), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
head(m2)
X1=m2[,1]
X2=m2[,2]
m2=cbind(X1,X2)

m3<-mvrnorm(n = 50, mu=c(9,6), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
head(m3)
X1=m3[,1]
X2=m3[,2]
m3=cbind(X1,X2)

#First way
plot(m1[,1], m1[,2], , col="darkorange", pch=1, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12),main='Left Panel Figure 2.8') 
points(m2[,1], m2[,2], , col="dodgerblue", pch=2, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12)) 
points(m3[,1], m3[,2], , col="darkgreen", pch=3, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12)) 

#Second way using former function from Chapter 1 ISLR

group=c(rep(1,50),rep(2,50),rep(3,50))
length(group)
data<-rbind(m1,m2,m3)
data1<-cbind(data,group)

Cols=function(vec){
  cols=rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))])
}

figure2_8_left=data1
plot(figure2_8_left[,1:2],col=Cols(figure2_8_left[,3]), pch=19, xlab="X1", ylab="X2",ylim=c(0,12),xlim=c(0,12),main='Left Panel Figure 2.8')

#Rigth Panel
m1<-mvrnorm(n = 50, mu=c(2.5,3), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m1[,1]
X2=m1[,2]
m1=cbind(X1,X2)

m2<-mvrnorm(n = 50, mu=c(2.5,6), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m2[,1]
X2=m2[,2]
m2=cbind(X1,X2)

m3<-mvrnorm(n = 50, mu=c(6,4), Sigma=matrix(c(1,0.5,0.5,1),2,2), tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
X1=m3[,1]
X2=m3[,2]
m3=cbind(X1,X2)

#First way
plot(m1[,1], m1[,2], , col="darkorange", pch=1, xlab="X1", ylab="X2", ylim=c(0,10),xlim=c(0,8),main='Righ Panel Figure 2.8') 
points(m2[,1], m2[,2], , col="dodgerblue", pch=2, xlab="X1", ylab="X2", ylim=c(0,10),xlim=c(0,8)) 
points(m3[,1], m3[,2], , col="darkgreen", pch=3, xlab="X1", ylab="X2", ylim=c(0,10),xlim=c(0,8)) 

#Second way using former function from Chapter 1 ISLR

group=c(rep(1,50),rep(2,50),rep(3,50))
data<-rbind(m1,m2,m3)
data1<-cbind(data,group)

Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

figure2_8_left=data1
plot(figure2_8_left[,1:2],col=Cols(figure2_8_left[,3]), pch=19, xlab="X1", ylab="X2",ylim=c(0,10),xlim=c(0,8),main='Righ Panel Figure 2.8')

