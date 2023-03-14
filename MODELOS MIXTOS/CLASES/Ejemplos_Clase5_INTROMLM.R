#EJEMPLOS CLASE 5 MLM

library(lattice)
library(ggplot2)
library(gridExtra)
library(Rcpp)
library(MASS)

data<-read.csv(file="SPRUCE1.csv",header=T,sep=',',dec='.')
head(data)
#Gráfico de perfiles promedio

ggplot(data, aes(x = time1, y = log_growth,group=Group)) + 
  stat_summary(aes(group=Group,col=Group),
               geom = "line", fun = mean, size = 1.8)+
  stat_smooth(aes(x = time1, y = log_growth,color=Group), method=NULL,size=0.1,alpha=0.2)+
  xlab("Tiempo")

# #Gráfico de perfiles promedio
# 
# ggplot(data, aes(x = time1, y = log_growth,group=Group)) + 
#   stat_summary(aes(group=Group,col=Group),
#                geom = "line", fun = mean, size = 1.8)+
#   stat_smooth(aes(x = time1, y = log_growth,color=Group), method=NULL,size=0.1,alpha=0.2)+
#   xlab("Tiempo")

#Gráfico de perfiles

ggplot(data, aes(x = time1, y = log_growth)) + 
  scale_colour_brewer(palette="Set1")+
  geom_point(size=2.8,alpha=0.3,aes(color = Group))+
  scale_shape_discrete(solid=FALSE)+
  geom_smooth(aes(x = time1, y = log_growth,col=Group),method="loess",size=2, span=0.5)+
  geom_text(
    label=data$ID, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap =TRUE
  )

#Gráfico de promedios y areas juntos

library(viridis)

data$Time<-as.factor(data$time1)

ggplot(data,aes(x = Time, y = log_growth,group=Group,fill=Group)) + 
  stat_summary(aes(group=Group,col=Group),
  geom = "area", fun = mean, size = 1.8, alpha=0.3,shade=TRUE)+
  scale_fill_viridis(discrete=TRUE)+
  xlab("Time")

data$Time<-as.factor(data$time1)
ggplot(data, aes(x = Time, y = log_growth)) + 
  geom_line(aes(color = Group, group = ID),alpha=0.5)+
  geom_point(size=2,aes(color = Group, group = ID),alpha=0.4)+
  geom_line(alpha=0)+
  scale_colour_brewer(palette="Set1")+
  stat_summary(aes(group=Group,col=Group),geom = "line", fun = mean, size = 2.5)+
  stat_summary(aes(group=Group,col=Group),geom = "point", fun = mean, size = 2.8,shape=21,fill="green")+
  xlab("Time")


#Perfiles promedio y perfiles de ambos grupos separados. Abetos

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

ggplot(data, aes(x = Time, y = log_growth))+
  geom_line(aes(group = ID),col="tomato",alpha=0.4)+
  stat_summary(aes(group = Group, color = Group),
               geom = "line", fun = mean, size = 3) +
  ylim(2,6.5)+
  facet_grid(.~Group)

#Gráfico de perfiles para sujetos seleccionados. Abetos

ggplot(subset(data, ID %in% c(1,3,5,7,13,14,20,22)), aes(x = Time, y = log_growth))+
  geom_line(aes(group = factor(ID)),col="tomato",alpha=0.4)+
  stat_summary(aes(group = factor(ID), color = factor(ID)),
               geom = "line", fun = identity, size = 3) +
  ylim(2,6.5)+
  facet_grid(.~Group)

#Gráfico de perfiles para sujetos seleccionados con su promedio. Abetos

ggplot(subset(data, ID %in% c(1,3,5,7,13,14,20,22)), aes(x = Time, y = log_growth))+
  geom_line(aes(group = ID),col="tomato",alpha=0.4)+
  stat_summary(aes(group = Group, color = Group),
               geom = "line", fun = mean, size = 3) +
  ylim(2,6.5)+
  facet_grid(.~Group)

#Gráfico de líneas de medias + boxplot. Abetos

Control<-data[data$Group=="Control",]
Ozone<-data[data$Group=="Ozone",]

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


ggplot(data, aes(x=factor(time1), y=log_growth,  fill=Group)) +
  geom_boxplot(alpha=0.5,position = "identity", width=0.5)+
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=Group,col=Group), geom="line", 
               size=2)+
  stat_summary(fun=mean, aes(group=Group,col=Group), geom="point", 
               size=2,col="yellow")+
  xlab("Time")

#Boxplor+Densidad

Control<-data[data$Group=="Control",]
Ozone<-data[data$Group=="Ozone",]

ggplot(data=Control, aes(x=log_growth)) +
  geom_density(aes(y=log_growth,group=factor(time1),fill="Control"),
               col="red",alpha=0.4,width=3.8,inherit.aes = FALSE)+
  geom_density(data=Ozone,aes(y=log_growth,group=factor(time1),fill="Ozone"),
               col="blue",alpha=0.3,width=3.8,inherit.aes = FALSE)+
  labs(fill = "Group",y="Log_Growth",x="")+
  geom_boxplot(data=Control,aes(y=log_growth,group=factor(time1),fill="Control"),alpha=0.5)+
  geom_boxplot(data=Ozone,aes(y=log_growth,group=factor(time1),fill="Ozone"),alpha=0.5)+
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggplot(data=Control, aes(x=log_growth)) +
  geom_density(aes(y=log_growth,group=factor(time1),fill=factor(time1)),
               col="red",alpha=0.4,width=3.8,inherit.aes = FALSE)+
  geom_density(data=Ozone,aes(y=log_growth,group=factor(time1),fill=factor(time1)),
               col="blue",alpha=0.3,width=3.8,inherit.aes = FALSE)+
  labs(fill = "Time",y="Log_Growth",x="")+
  geom_boxplot(data=Control,aes(y=log_growth,group=factor(time1),fill=factor(time1)),alpha=0.5)+
  geom_boxplot(data=Ozone,aes(y=log_growth,group=factor(time1),fill=factor(time1)),alpha=0.5)+
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Boxplot + density + scatterplot. Abetos
library(ggside)
#library(tidyverse)
library(tidyquant)


###Violin Boxplot + Density + Scatter
###By Group

l1<-ggplot(data, aes(x=factor(time1), y=log_growth, color=Group, group=Group,fill=Group))+
  geom_point(alpha=0.6,size=3,shape=19)+ 
  xlab("Time")

l1 + geom_ysidedensity(aes(y = log_growth, group=Group,col=Group,fill=Group,
                          x = after_stat(density)),alpha= 0.5,size = 1,inherit.aes=FALSE)+
  geom_xsideviolin(aes(y = log_growth, group=Group,col=Group,fill=Group),
                   orientation = "x", alpha=0.4,inherit.aes = TRUE)+
  theme_tq() +
  scale_ysidex_discrete(labels=NULL)+
  theme(ggside.panel.scale = .4)

l1<-ggplot(data, aes(x = Time, y = log_growth,group=Group,fill=Group,colour=Group)) + 
  stat_summary(aes(group=Group,col=Group),geom = "line", fun = mean, size = 1.8)+
  stat_summary(aes(group=Group,col=Group),geom = "point", fun = mean, size = 2.8)+
  xlab("Time")
l1+ geom_ysidedensity(aes( x    = after_stat(density)),alpha= 0.5,size = 1)+
  geom_ysideboxplot(aes(y = log_growth, group=Group,col=Group,fill=Group),
                    orientation = "x", alpha=0.6,inherit.aes = TRUE)+
  theme_tq() +
  labs(title = "Spruce Sitka growth lineplot" ,
       subtitle = "Density Plot+Boxplot",
       x = "Time", y = "Log-Growth")+
  theme(ggside.panel.scale = .4)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

l2<-ggplot(data, aes(x = Group, y = log_growth,fill=Time,group=Time)) + 
  stat_summary(aes(group=Time,col=Time),
               geom = "line", fun = mean, size = 1.8)+
  stat_summary(aes(group=Time,col=Time),
               geom = "point", fun = mean, size = 2.8)+
  xlab("Group")

#Usando la libreria ggside:

l2+ geom_ysidedensity(aes( x    = after_stat(density)),alpha= 0.5,size = 1)+
  geom_ysideboxplot(aes(y = log_growth, group=Time,col=Time,fill=Time),
                    orientation = "x", alpha=0.6,inherit.aes = TRUE)+
  theme_tq() +
  labs(title = "Spruce Sitka growth lineplot" ,
       subtitle = "Density Plot+Boxplot",
       x = "Group", y = "Log-Growth")+
  theme(ggside.panel.scale = .4)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

l1<-ggplot(data, aes(x=factor(time1), y=log_growth,  fill=Group)) +
  geom_boxplot(alpha=0.5,position = "identity", width=0.5)+
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=Group,col=Group), geom="line", size=2)+
  stat_summary(fun=mean, aes(group=Group,col=Group), geom="point", size=2,col="yellow")+
  xlab("Time")

l1+ geom_ysidedensity(aes(y = log_growth, group=Group,col=Group,fill=Group,
                          x    = after_stat(density)),
                      alpha= 0.5,size = 1,inherit.aes=FALSE)+
  geom_ysideboxplot(aes(y = log_growth, group=Group,col=Group,fill=Group),
                    orientation = "x", alpha=0.4,inherit.aes = TRUE)+
  theme_tq() +
  scale_ysidex_discrete(labels=NULL)+
  theme(ggside.panel.scale = .4)

l1<-ggplot(data, aes(x=Group, y=log_growth,  fill=Time)) +
  geom_boxplot(alpha=0.3,position = "identity", width=0.1)+
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=Time,col=Time), geom="line", size=2)+
  stat_summary(fun=mean, aes(group=Time,col=Time), geom="point", size=2,
               col="yellow")+
  xlab("Group")

l1+ geom_ysidedensity(aes(y = log_growth, group=Time,col=Time,fill=Time,
                          x    = after_stat(density)),
                      alpha= 0.3,size = 1,inherit.aes=FALSE)+
  geom_ysideboxplot(aes(y = log_growth, group=Time,col=Time,fill=Time),
                    orientation = "x", alpha=0.4,inherit.aes = TRUE)+
  theme_tq() +
  scale_ysidex_discrete(labels=NULL)+
  theme(ggside.panel.scale = .4)

l3<-ggplot(data, aes(x=factor(time1), y=log_growth,  fill=Group)) +
  geom_violin(alpha=0.5,position = "identity", width=0.5)+
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=Group,col=Group), geom="line", size=2)+
  stat_summary(fun=mean, aes(group=Group,col=Group), geom="point", size=2,col="yellow")+
  xlab("Time")

l3+ geom_ysidedensity(aes(y = log_growth, group=Group,col=Group,fill=Group,
                          x    = after_stat(density)),
                      alpha= 0.5,size = 1,inherit.aes=FALSE)+
  geom_ysideviolin(aes(y = log_growth, group=Group,col=Group,fill=Group),
                   orientation = "x", alpha=0.4,inherit.aes = TRUE)+
  theme_tq() +
  scale_ysidex_discrete(labels=NULL)+
  theme(ggside.panel.scale = .4)


l4<-ggplot(data, aes(x=Group, y=log_growth,  fill=Time)) +
  geom_violin(alpha=0.3,position = "identity", width=0.5)+
  geom_line(alpha=0)+
  stat_summary(fun=mean, aes(group=Time,col=Time), geom="line", size=2)+
  stat_summary(fun=mean, aes(group=Time,col=Time), geom="point", size=2,
               col="yellow")+
  xlab("Group")

l4+ geom_ysidedensity(aes(y = log_growth, group=Time,col=Time,fill=Time,
                          x    = after_stat(density)),
                      alpha= 0.3,size = 1,inherit.aes=FALSE)+
  geom_ysideviolin(aes(y = log_growth, group=Time,col=Time,fill=Time),
                   orientation = "x", alpha=0.4,inherit.aes = TRUE)+
  theme_tq() +
  scale_ysidex_discrete(labels=NULL)+
  theme(ggside.panel.scale = .4)



