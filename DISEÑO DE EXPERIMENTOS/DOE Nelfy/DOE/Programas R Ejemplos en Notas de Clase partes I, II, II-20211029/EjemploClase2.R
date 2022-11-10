#--------------Modleos de compraciones simples--------------

rm(list=ls(all=TRUE))
datos4=data.frame(Temperatura=factor(rep(c("baja","alta"),
                                         times=10)),encogimiento=scan())
17.2 21.4
17.5 20.9
18.6 19.8
15.9 20.4
16.4 20.6
17.3 21.0
16.8 20.8
18.4 19.9
16.7 21.1
17.6 20.3

attach(datos4)
medias=sapply(split(encogimiento,Temperatura),mean)
medias
mean(encogimiento) #promedio global
vari=sapply(split(encogimiento,Temperatura),var); vari
boxplot(encogimiento~Temperatura,boxwex=0.5)

#Test Varianzas
var.test(encogimiento~Temperatura,alternative="two.sided")

#Diferencia de medias h0: µ1 = µ2
t.test(encogimiento~Temperatura,var.equal=TRUE,
       alternative="two.sided",paired=FALSE)

#otra forma
#library(gmodels)
#fit.contrast(modeloanova,"Temperatura",
#             rbind(":Alto menos Bajo"=c(1,-1)),conf=0.95)

#Modelo Anova
modeloanova=aov(encogimiento~Temperatura)
summary(modeloanova)

#Anova con variable factor
library(lsmeans)
lsmeans(modeloanova,~Temperatura)


#Solucion mediante modelo de regresion con medias de 
#tratamientos: Asume la igualdad de varianzas.

mrlm1=lm(encogimiento~-1+Temperatura)
summary(mrlm1)
confint(mrlm1)
library(car)
linearHypothesis(mrlm1,c("Temperaturaalta-Temperaturabaja=0"))
I1=ifelse(Temperatura=="alta",1,0); I2=ifelse(Temperatura=="baja",1,0)
X=I1-I2 #desde que n_1=n_2
mrlm2=lm(encogimiento~X); summary(mrlm2)

I1=ifelse(Temperatura=="alta",1,0); I2=ifelse(Temperatura=="baja",1,0)
X=I1-I2 #desde que n_1=n_2
mrlm2=lm(encogimiento~X)
anova(mrlm2)

##########################################

#=====Residuos Estudentizados===========

#GRÁFICOS DE RESIDUOS ESTUDENTIZADOS INTERNAMENTE
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
plot(fitted(modeloanova),rstandard(modeloanova),main="residuos estudentizados 
     vs. ajustados\nModelo ANOVA",
     pch=as.numeric(Temperatura),col=as.numeric(Temperatura),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("top",legend=c(expression(paste(T^o,sep=" ","alta")),
                      expression(paste(T^o,sep=" ","baja"))),
       pch=1:2,col=1:2,bg="cornsilk")
stripchart(rstandard(modeloanova)~Temperatura,
           main="residuos estudentizados vs. Temperatura\nModelo ANOVA",
           xlab="Temperatura",vertical=T, pch=1:2,col=1:2,cex=1.5)
abline(h=c(-2,0,2),lty=2)
plot(fitted(mrlm2),rstandard(mrlm2),
     main="residuos estudentizados vs. ajustados\nModelo de efectos con MRL",
     pch=as.numeric(Temperatura),col=as.numeric(Temperatura),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("top",legend=c(expression(paste(T^o,sep=" ","alta")),
                      expression(paste(T^0,sep=" ","baja"))),
       pch=1:2,col=1:2,bg="cornsilk")
plot(X,rstandard(mrlm2),
     main="residuos estudentizados vs. X\nModelo de efectos con MRL",
     xlab="X",pch=as.numeric(Temperatura),col=as.numeric(Temperatura),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("top",legend=c(expression(paste(T^o,sep=" ","alta",
                                       sep=" ","(",sep="",X==+1,sep="",")")),
                      expression(paste(T^o,sep=" ","baja",
                                       sep=" ","(",sep="",X==-1,sep="",")"))),
       pch=1:2,col=1:2,bg="cornsilk")

