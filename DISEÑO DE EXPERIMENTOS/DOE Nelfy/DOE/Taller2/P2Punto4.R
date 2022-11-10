#Parte II punto 4

rm(list=ls(all=TRUE))
library(daewr)
library(lme4)
library(MASS)
source("FUNCIONESDEUSUARIOPARAMODELOSDOSFACTORES.R")

problema4=data.frame(Máquina=factor(rep(c("M1","M2","M3","M4"),each=8)), 
                     Día=factor(rep(c("D1","D1","D2","D2","D3","D3","D4","D4"),
                                    times=4)),productividad=scan())
142.3 144.0 134.9 146.3 148.6 156.5 152.0 151.4
148.6 146.9 145.2 146.3 148.6 153.1 149.7 152.0
142.9 147.4 125.9 127.6 135.5 138.9 142.9 142.3
133.8 133.2 108.9 107.5 132.1 149.7 141.7 141.2

problema4
attach(problema4)

interaction.plot(Máquina,Día,productividad,type="b",lwd=2,col=1:4,pch=1:4) 
interaction.plot(Día,Máquina,productividad,type="b",lwd=2,col=1:4,pch=1:4)
modeloaux=aov(productividad~Máquina*Día) #correr modelo como si fuese de efectos fijos 
Anovafactorialaleatoriosconinteraccion(modelo=modeloaux) #Anova correcta considerando efectos aleatorios
                                                         #con interacción

#sumas de cuadrados y cuadrado medios modelo de efectos aleatorios con interacción 
modelo=aov(productividad~Error(Máquina*Día))
summary(modelo)

#componentes de varianza modelo de efectos aleatorios con interacción 
modelob=lmer(productividad~1+(1|Máquina)+(1|Día)+(1|Máquina:Día)) 
summary(modelob)
shapiro.test(rstandard(modeloaux))


#OBTENIENDO GRÁFICOS DE RESIDUOS ESTUDENTIZADOS,
layout(rbind(c(1,1,2,2),c(3,3,4,4))) 
stripchart(rstandard(modeloaux)~Máquina,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="Máquina") 
abline(h=c(-2,0,2),lty=2) 
stripchart(rstandard(modeloaux)~Día,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="Día") 
abline(h=c(-2,0,2),lty=2)
plot(fitted(modeloaux),rstandard(modeloaux),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(modeloaux))
qqline(rstandard(modeloaux),lty=2)
legend("topleft",legend=c("Shapiro-Wilk Test",expression(W==0.90151),expression(PValue== 0.00677)),cex=1.1)

detach(problema4)

