#Ejemplos clase 16 MPYML
library(lattice)
library(ggplot2)
library(gridExtra)
library(Rcpp)
library(MASS)
library(lattice)
library(nlme)
library(lme4)
library(lmerTest)
library(MCMCglmm)
library(MCMCpack)


data<-read.csv(file="SPRUCE1.csv",header=T,sep=',',dec='.')
#data$ID<-data$ï..ID
head(data)

#Gráfico de perfiles

ggplot(data, aes(x = time1, y = log_growth,group=ID)) + 
  geom_line(aes(color = Group, group = ID))+
  scale_colour_brewer(palette="Set1")+
  geom_point(size=2,aes(color = Group, group = ID))

#Gráfico de promedios

ggplot(data, aes(x = time1, y = log_growth)) + 
  geom_line(alpha=0)+
  scale_colour_brewer(palette="Set1")+
  stat_summary(aes(group=Group,col=Group),
               geom = "line", fun = mean, size = 1.2)+
  stat_summary(aes(group=Group,col=Group),
               geom = "point", fun = mean, size = 2.8,shape=19)+
  xlab("Time")

#Gráfico de promedios y perfiles juntos

ggplot(data, aes(x = time1, y = log_growth)) + 
  geom_line(aes(color = Group, group = ID),alpha=0.5)+
  geom_point(size=2,aes(color = Group, group = ID),alpha=0.4)+
  geom_line(alpha=0)+
  scale_colour_brewer(palette="Set1")+
  stat_summary(aes(group=Group,col=Group),
               geom = "line", fun = mean, size = 2.5)+
  stat_summary(aes(group=Group,col=Group),
               geom = "point", fun = mean, size = 2.8,shape=21,fill="green")+
  xlab("Time")


Control<-data[data$Group=="Control",]
Ozone<-data[data$Group=="Ozone",]

Cont<-ggplot(Control, aes(x = time1, y = log_growth))+
  geom_line(aes(color = Group, group = ID))+
  stat_summary(aes(group = Group, color = paste("mean", Group)),
               geom = "line", fun = mean, size = 3) +
  scale_colour_manual(name = "Group",values = c("Control" = "#F8766D", "mean Control" = "blue"))+
  ylim(2,6.5)


Ozon<-ggplot(Ozone, aes(x = time1, y = log_growth))+
  geom_line(aes(color = Group, group = ID))+
  stat_summary(aes(group = Group, color = paste("mean", Group)),
               geom = "line", fun = mean, size = 3) +
  scale_colour_manual(name = "Group",values = c("Ozone" = "#F8766D", "mean Ozone" = "blue"))+
  ylim(2,6.5)

grid.arrange(Cont,Ozon,nrow=1)

#Global boxplot ignoring longitudinality
means1<-round(c(by(data$log_growth,data$Group,mean)),3)
ggplot(data,aes(x=Group,y=log_growth, group=Group,col=Group,fill=Group))+
  geom_boxplot(col="red")+
  stat_summary(aes(x=Group,y=log_growth, group=Group,col=Group,fill=Group),
               fun=mean,
               col="yellow",size=0.7)+
  geom_text(aes(x="Control",label=paste0("Mean\n",means1[1]), y=means1[1]),size=3,col="black")+
  geom_text(aes(x="Ozone",label=paste0("Mean\n",means1[2]), y=means1[2]),size=3,col="black")

#Global density plots ignoring longitudinality
means1<-round(c(by(data$log_growth,data$Group,mean)),3)
ggplot(data,aes(x=log_growth, group=Group,col=Group,fill=Group))+
  geom_density(col="red",alpha=0.5)+
  geom_vline(xintercept=means1[1], size=0.9, color="red",alpha=0.54)+
  geom_vline(xintercept=means1[2], size=0.9, color="blue",alpha=0.54)+
  geom_text(aes(x=means1[1],label=paste0("Mean\n",means1[1]), y=0.65),size=3,col="tomato")+
  geom_text(aes(x=means1[2],label=paste0("Mean\n",means1[2]), y=0.72),size=3,col="blue")+
  ylim(0,0.75)+
  xlim(1.8,7)

resu1<-lmer(log_growth~Group+time1+(1|ID),data=data)
summary(resu1)

data$fitted_vals<-fitted(resu1)
data

sujetos<-data[data$ID==1 | data$ID==14,]

ggplot(sujetos, aes(x = time1, y = log_growth)) + 
  geom_line(aes(color = Group, group = ID),linetype="dashed")+
  geom_line(aes(y=fitted_vals,group = ID,color = Group))+
  scale_colour_brewer(palette="Set1")+
  geom_point(size=2,aes(color = Group, group = ID))



########Ejemplo 2 AFH######################

datos<-read.csv("datos_simul_AFH.csv",header=TRUE,sep=",",dec=".")
head(datos)
tail(datos)
#Global boxplot ignoring longitudinality
means2<-round(c(by(datos$afh,datos$gender,mean)),3)
ggplot(datos,aes(x=factor(gender),y=afh, group=factor(gender),col=factor(gender),fill=factor(gender)))+
  geom_boxplot(col="red")+
  stat_summary(aes(x=factor(gender),y=afh, group=factor(gender),col=factor(gender),fill=factor(gender)),
               fun=mean,
               col="yellow",size=0.7)+
  geom_text(aes(x="0",label=paste0("Mean\n",means2[1]), y=means2[1]),size=3,col="black")+
  geom_text(aes(x="1",label=paste0("Mean\n",means2[2]), y=means2[2]),size=3,col="black")

#Global density plots ignoring longitudinality
ggplot(datos,aes(x=afh,group=factor(gender),col=factor(gender),fill=factor(gender)))+
  geom_density(col="red",alpha=0.5)+
  geom_vline(xintercept=means2[1], size=0.9, color="red",alpha=0.54)+
  geom_vline(xintercept=means2[2], size=0.9, color="blue",alpha=0.54)+
  geom_text(aes(x=means2[1],label=paste0("Mean\n",means2[1]), y=0.04),size=3,col="tomato")+
  geom_text(aes(x=means2[2],label=paste0("Mean\n",means2[2]), y=0.044),size=3,col="blue")+
  ylim(0,0.045)+
  xlim(50,130)

gg.base <- ggplot(datos, aes(x = age, y = afh))
gg.base + stat_summary(aes(group = factor(gender), color = factor(gender)),
                       geom = "line", fun = mean, size = 3)

gg.Gline <- gg.base + geom_line(aes(color = factor(gender), group = id))
gg.Gline + stat_summary(aes(group = factor(gender), color = paste("mean", gender)),
                        geom = "line", fun = mean, size = 3) +
  scale_colour_manual(name = "Group",values = c("1" = "#F8766D", "mean 1" = "#F8766D",
                                                "0" = "#00BFC4", "mean 0" = "#00BFC4"))
par(mfrow=c(1,3))
boxplot(afh ~ gender,col=c("#F8766D","#00BFC4"),data=datos)
boxplot(afh ~ age,col=c("#F8766D","#00BFC4"),data=datos)
boxplot(afh ~ age*gender,col=c("#F8766D","#00BFC4"),data=datos)
#dev.off()

#Otra version del Boxplot by time and group
head(datos)
Female<-datos[datos$gender==0,]
Male<-datos[datos$gender==1,]

ggplot(data=Female, aes(x=factor(age), y=afh)) +
  geom_boxplot(aes(x=factor(age), y=afh,group=factor(age),fill="Female"),
               col="red",alpha=0.4)+
  geom_boxplot(data=Male,aes(x=factor(age), y=afh,group=factor(age),fill="Male"),
               col="blue",alpha=0.3)+
  stat_summary(data=Female,fun=mean, geom="point", size=2,col="red",shape=19)+
  stat_summary(data=Male,fun=mean, geom="point", size=2,col="blue",shape=19)+
  labs(fill = "Gender",y="AFH",x="Age")

#Gráfico de líneas de medias
ggplot(datos, aes(x=age, y=afh,group=factor(gender),col=factor(gender))) +
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=factor(gender)), geom="line", size=2)+
  xlab("Time")+
  stat_summary(fun=mean, geom="point", size=4,shape=19)


#Gráfico de líneas de medias + boxplot

ggplot(data=Female, aes(x=factor(age), y=afh)) +
  geom_boxplot(aes(x=factor(age), y=afh,group=factor(age),fill="Female"),
               col="red",alpha=0.4)+
   geom_boxplot(data=Male,aes(x=factor(age), y=afh,group=factor(age),fill="Male"),
               col="blue",alpha=0.3,width=1)+
  labs(fill = "Gender",y="AFH",x="Age") +
  geom_line(data=Male,alpha=0,inherit.aes=T)+
  stat_summary(data=Male,fun=mean, aes(group=factor(gender)), geom="line", size=1.1,col="blue")+
  stat_summary(data=Male,fun=mean, geom="point", size=2,shape=19,col="yellow")+
  geom_line(data=Female,alpha=0,inherit.aes=T)+
  stat_summary(data=Female,fun=mean, aes(group=factor(gender)), geom="line", size=1.1,col="tomato")+
  stat_summary(data=Female,fun=mean, geom="point", size=2,shape=19,col="red")

####

dev.off()

resu<-suppressMessages(lmer(afh~gender+age+(1|id),data=datos))
summary(resu)
require(lme4)
qqnorm(residuals(resu))
datos$fitted_lmer<-fitted(resu)
#head(datos)
#Otra forma usando ggplot
df1<-data.frame(residual=residuals(resu))

p <- ggplot(df1, aes(sample = residual))+ 
  stat_qq(col="blue",shape=1,size=2) + 
  stat_qq_line(col="red",size=1.5)
p 

#Para predecir valores a los 15 años con el modelo ajustado

datos$predict = predict(resu)
head(datos)
newDat<-data.frame(id=c(33,16),age=c(15,15),gender=c("1","0"))
predict(resu,newdata=newDat)

##################
set.seed(1234)
resu2<-MCMChregress(fixed=afh~as.factor(gender)+age, random=~1, group='id', data=datos, 
                    burnin=1000, mcmc=10000, thin=10, verbose=0, seed=NA, 
                    beta.start=NA,sigma2.start=NA, Vb.start=NA, mubeta=0, 
                    Vbeta=1.0E6, r=3,R=20.09, nu=0.001, delta=0.001)
a<-as.vector(summary(resu2$mcmc))
a[1:4]$statistics[1:3,1:2]

####Plotting fitted model with lmer

#Gráfico de líneas para tres sujetos de la base original

sujetos<-datos[datos$id==446 | datos$id==467  | datos$id==482,]

ggplot(sujetos, aes(x = age, y = afh)) + 
  geom_line(aes(color = factor(gender), group = id),linetype="dashed")+
  geom_line(aes(y=fitted_lmer,group = id,color = factor(gender)))+
  scale_colour_brewer(palette="Set1")+
  geom_point(size=2,aes(color = factor(gender), group = id))

#Gráfico de líneas de medias

ggplot(datos, aes(x = age, y = afh,group=factor(gender),col=factor(gender))) + 
  #geom_line(aes(color = factor(gender), group = factor(gender)))+
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=factor(gender),color = factor(gender)), geom="line",linetype="dashed", size=2)+
  geom_line(aes(y=fitted_lmer,group = factor(gender),color = factor(gender)),alpha=0)+
  stat_summary(fun=mean, aes(group=factor(gender),color = factor(gender)), geom="line", size=1)+
  scale_colour_brewer(palette="Set2")

