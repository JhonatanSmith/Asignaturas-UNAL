#EJEMPLO CLASE 9 MLM

library(lattice)
library(ggplot2)
library(gridExtra)
library(Rcpp)
library(MASS)
library(nlme)

data<-read.csv(file="SPRUCE1.csv",header=T,sep=',',dec='.')
head(data)

by(data$log_growth,data$Group,summary)
by(data$log_growth,data$time1,summary)

#Gráfico de perfiles promedio

data$Time<-as.factor(data$time1)
a<-ggplot(data, aes(x = Time, y = log_growth)) + 
  geom_line(aes(color = Group, group = ID),alpha=0.5)+
  geom_point(size=2,aes(color = Group, group = ID),alpha=0.4)+
  geom_line(alpha=0)+
  scale_colour_brewer(palette="Set1")+
  stat_summary(aes(group=Group,col=Group),geom = "line", fun = mean, size = 2.5)+
  stat_summary(aes(group=Group,col=Group),geom = "point", fun = mean, size = 2.8,shape=21,fill="green")+
  xlab("Time")

b<-ggplot(data, aes(x=Time, y=log_growth,  fill=Group)) +
  geom_boxplot(alpha=0.5)

grid.arrange(b,a,nrow=2)

#Fitting a model with random intercept and compound symmetry

mod1<-lme(log_growth~Time+Group+Time*Group, random=~1|ID, 
          corr=corCompSymm(form=~1|ID), data=data)
summary(mod1)

anova(mod1, type = "marginal")
