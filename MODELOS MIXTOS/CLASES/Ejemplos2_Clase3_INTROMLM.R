#Ejemplos Clase 3 DLMR

#1) Datos Me_theta

library(ggplot2)
library(gridExtra)
library(questionr)
library(lattice) 
library(psych)

data<-read.csv(file="NEW_DATASR_ENSAYO_AUTOMATIZACION.csv",header=T,sep=';',dec=',')
head(data)
plot(data$age[data$id==21],data$me_theta[data$id==21],type="b",col=c("black"),ylim=c(60,75),
     xlim=c(6,11),lwd=2,lty = 2,main="Observed growth trajectories (growth profiles) 
     for Me-Theta",ylab="Me-Theta",xlab="AGE(years)")
text(10.4, 67.3, "ID21", cex = .8, col="black")
cl <- rainbow(6)
for (i in 22:27){
  lines(data$age[data$id==i],data$me_theta[data$id==i],type="b",col=cl[28-(i)],ylim=c(60,75),
        xlim=c(6,11),lwd=2,lty = 2,main="Observed growth trajectories (growth profiles) 
        for Me-Theta",ylab="Me-Theta",xlab="AGE(years)")
}

#Usando GGplot

ggplot(data=data,aes(x=age,y=me_theta,group=factor(id),color=factor(id),fill=factor(id)))+
  geom_point(aes(x=age,y=me_theta,group=factor(id)))+
  geom_line(linetype = "dashed")+
  labs(y="Me_theta",x="Age")+
  stat_summary(aes(x=age,y=me_theta,group=1),
               geom = "line", 
               col="red",
               fun = mean, size = 1.7)+
  ggtitle("Me-theta angle for 7 French-Canadian girls. Red solid line is the mean")

###########################

#2) Datos árboles Spruce-Sitka

#spruce<-read.csv(file="Base_de_datos_Anglosajonas.csv",header=T,sep=';',dec=',')
spruce<-read.csv(file="SPRUCE1.csv",header=T,sep=',',dec='.')

head(spruce)
#spruce$ID<-spruce$ï..ID
by(spruce$log_growth, spruce$ID, summary)
by(spruce$log_growth,spruce$Group,summary)
by(spruce$log_growth,spruce$time1,summary)

aggregate(log_growth~Group+time1, data = spruce, FUN= "summary" )
means<-aggregate(log_growth~Group+time1, data = spruce, FUN= "mean")
sdev<-aggregate(log_growth~Group+time1, data = spruce, FUN= "sd")
sdev<-sdev[,3]
mean_sd<-cbind(means,sdev)
describeBy(spruce, group=spruce$Group)
describeBy(spruce, group=spruce$time1)

#Global boxplot ignoring longitudinality
means1<-round(c(by(spruce$log_growth,spruce$Group,mean)),2)
ggplot(spruce,aes(x=Group,y=log_growth, group=Group,col=Group,fill=Group))+
         geom_boxplot(col="red")+
  stat_summary(aes(x=Group,y=log_growth, group=Group,col=Group,fill=Group),
               fun=mean,
               col="purple",size=1)+
  geom_text(aes(x="Control",label=paste0("Mean\n",means1[1]), y=means1[1]),size=3,col="black")+
  geom_text(aes(x="Ozone",label=paste0("Mean\n",means1[2]), y=means1[2]),size=3,col="black")

#Boxplot by time

ggplot(spruce, aes(x=time1, y=log_growth,  col=factor(time1),group=factor(time1))) +
  geom_boxplot(aes(x=time1, y=log_growth),fill="yellow",alpha=0.5)

#Boxplot by time and group

ggplot(spruce, aes(x=factor(time1), y=log_growth,  fill=Group)) +
  geom_boxplot(alpha=0.5)

#Otra version del Boxplot by time and group

Control<-spruce[spruce$Group=="Control",]
Ozone<-spruce[spruce$Group=="Ozone",]

ggplot(data=Control, aes(x=time1, y=log_growth)) +
  geom_boxplot(aes(x=time1, y=log_growth,group=factor(time1),fill="Control"),
               col="red",alpha=0.4,width=3.8)+
  geom_boxplot(data=Ozone,aes(x=time1, y=log_growth,group=factor(time1),fill="Ozone"),
               col="blue",alpha=0.3,width=3.8)+
  stat_summary(data=Control,fun=mean, geom="point", size=2,col="red",shape=19)+
  stat_summary(data=Ozone,fun=mean, geom="point", size=2,col="blue",shape=19)+
  labs(fill = "Group",y="Log_Growth",x="Time")

#Gráfico de líneas simple
ggplot(spruce, aes(x=time1, y=log_growth,group=factor(ID),col=Group)) + 
  geom_line(size=1.1)+
  geom_point(aes(x=time1, y=log_growth,group=factor(ID),col=Group),size=2, shape=2)

#Gráfico de líneas de medias 
ggplot(spruce, aes(x=time1, y=log_growth,group=Group,col=Group)) +
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=Group), geom="line", size=2)+
  xlab("Time")+
  stat_summary(fun=mean, geom="point", size=4,shape=19)


#Gráfico de líneas de medias + boxplot

# ggplot(spruce, aes(x=factor(time1), y=log_growth,  fill=Group)) +
#   geom_boxplot(alpha=0.5)+
#   geom_line(alpha=0)+
#   stat_summary(fun=mean, aes(group=Group,col=Group), geom="line", size=2)+
#   xlab("Time")

ggplot(data=Control, aes(x=time1, y=log_growth)) +
  geom_boxplot(aes(x=time1, y=log_growth,group=factor(time1),fill="Control"),
               col="red",alpha=0.4,width=3.8)+
  geom_boxplot(data=Ozone,aes(x=time1, y=log_growth,group=factor(time1),fill="Ozone"),
               col="blue",alpha=0.3,width=3.8)+
  labs(fill = "Group",y="Log_Growth",x="Time")+
  geom_line(alpha=0)+
  geom_line(data=Control,aes(group=Group),alpha=0)+
  stat_summary(data=Control,fun=mean, geom="line", size=1.2,color="tomato")+
  stat_summary(data=Control,fun=mean, geom="point", size=2,col="red",shape=19)+
  geom_line(alpha=0)+
  geom_line(data=Ozone,aes(group=Group),alpha=0)+
  stat_summary(data=Ozone,fun=mean, geom="line", size=1.2,color="lightblue")+
  stat_summary(data=Ozone,fun=mean, geom="point", size=2,col="blue",shape=19)


# lattice library: xyplot function

C<-xyplot(log_growth ~ time1 | as.factor(ID), data=Control, as.table=T)
O<-xyplot(log_growth ~ time1 | as.factor(ID), data=Ozone, as.table=T)

grid.arrange(C,O)

C1<-suppressWarnings(xyplot(log_growth ~ time1 | as.factor(ID), data=Control,
       prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
       xlab = "Time", ylab = "Log(Growth)",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian")
         panel.text(x=x,y=y,labels=y,pos=1,cex=0.5)},
       ylim=c(0, 8), as.table=T))

O1<-suppressWarnings(xyplot(log_growth ~ time1 | as.factor(ID), data=Ozone,
           prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
           xlab = "Time", ylab = "Log(Growth)",
           panel = function(x, y) {
             panel.xyplot(x, y)
             panel.loess(x,y, family="gaussian")
             panel.text(x=x,y=y,labels=y,pos=1,cex=0.5)},
           ylim=c(0, 8), as.table=T))

grid.arrange(C1,O1)

head(spruce)

bwplot(log_growth ~ Group | as.factor(time1),data=spruce)

