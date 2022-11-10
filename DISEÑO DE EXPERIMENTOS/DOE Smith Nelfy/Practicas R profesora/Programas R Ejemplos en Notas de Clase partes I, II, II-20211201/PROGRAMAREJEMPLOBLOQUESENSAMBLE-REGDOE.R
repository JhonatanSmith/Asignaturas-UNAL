library(gmodels)
library(multcomp)
library(lsmeans)
library(agricolae)
#ejemplo Sección 7.6 Notas DOE
ensamble=data.frame(Método=factor(rep(c("A","B","C","D"),times=4)),Operador=factor(rep(1:4,each=4)),Tiempo=scan())
6 7 10 10
9 10 16 13
7 11 11 11
8 8 14 9

ensamble

attach(ensamble)

mediasMét=sapply(split(Tiempo,Método),mean)
mediasOpe=sapply(split(Tiempo,Operador),mean)

win.graph(width=7,height=3.5)
layout(matrix(c(1,1,2,2),nrow=1))
boxplot(Tiempo~Método,boxwex=0.4,xlab="Método")
lines(mediasMét,type="b",pch=19,col=2,lty=2)
boxplot(Tiempo~Operador,boxwex=0.4,xlab="Operador")
lines(mediasOpe,type="b",pch=19,col=2,lty=2)

#Gráfico de Medias
plot.design(Tiempo~Método+Operador)

#AJUSTANDO EL MODELO ANOVA Y OBTENCIÓN DE LA TABLA ANOVA
diseño=aov(Tiempo~Método+Operador)
anova(diseño)

#OBTENCIÓN DE MEDIAS DE TRATAMIENTO CON SUS I.C DEL 95%
lsmeans(diseño,"Método")

#OBTENIENDO EFECTOS DE TRATAMIENTOS, RESULTADOS PARA TEST DE SIGNIFICANCIA Y SUS I.C DEL 95%
efect.métodoA=fit.contrast(diseño,"Método",rbind(":efecto método A"=c(3/4,-1/4,-1/4,-1/4)),conf=0.95)
efect.métodoB=fit.contrast(diseño,"Método",rbind(":efecto método B"=c(-1/4,3/4,-1/4,-1/4)),conf=0.95)
efect.métodoC=fit.contrast(diseño,"Método",rbind(":efecto método C"=c(-1/4,-1/4,3/4,-1/4)),conf=0.95)
efect.métodoD=fit.contrast(diseño,"Método",rbind(":efecto método D"=c(-1/4,-1/4,-1/4,3/4)),conf=0.95)
rbind(efect.métodoA,efect.métodoB,efect.métodoC,efect.métodoD)

#INTERVALOS DE TUKEY PARA LAS DIFERENCIAS DE MEDIAS DEL FACTOR DE TRATAMIENTOS
TukeyHSD(diseño,"Método",conf.level=0.95)
HSD.test(diseño,"Método", group=TRUE,console=TRUE) #Comparaciones de Tukey

#GRÁFICOS DE INTERVALOS DE TUKEY
plot(TukeyHSD(diseño,"Método",conf.level = 0.95),cex.lab=0.8,las=1)

#OBTENIENDO TEST DE NORMALIDAD SOBRE RESIDUALES ESTUDENTIZADOS INTERNAMENTE
shapiro.test(rstandard(diseño))


#OBTENIENDO DE GRÁFICOS PARA VALIDACIÓN DE SUPUESTOS CON RESIDUOS ESTUDENTIZADOS INTERNAMENTE
nf=layout(rbind(c(1,1,2,2),c(3,3,4,4)))
plot(fitted(diseño),rstandard(diseño),ylim=c(-2.5,2.5),cex=1.5,main="Residuales estudentizados vs. valores ajustados")
abline(h=c(-2,0,2),col=2)

stripchart(rstandard(diseño)~Método,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1.5,xlab="Método",main="Residuales estudentizados vs. método")
abline(h=c(-2,0,2),col=2)

stripchart(rstandard(diseño)~Operador,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1.5,xlab="Operador",main="Residuales estudentizados vs. operador")
abline(h=c(-2,0,2),col=2)

qqnorm(rstandard(diseño),cex=1.5)
qqline(rstandard(diseño),col=2,lty=2)
legend("topleft",legend=c("Shapiro-Wilk Test",expression(W==0.97299),expression(PValue== 0.8844)),cex=1.1)

detach(ensamble)

##########################
#Problema propuesto Sección 7.7 Notas DOE
library(gmodels)
library(multcomp)
library(lsmeans)
library(agricolae)

ensamble2=data.frame(operador=factor(rep(1:6,times=3)),montaje=factor(rep(c("A","B","C"),each=6)),tiempo=scan())
20.2 22.6 19.2 22.5 18.7 21.5
23.7 24.1 22.6 24.3 20.2 21.5
21.4 23.0 22.9 22.0 19.8 20.1

ensamble2

attach(ensamble2)
layout(c(1,1,2,2))
plot(tiempo~operador+montaje,boxwex=0.5)
win.graph()
plot.design(tiempo~operador+montaje)

diseño2=aov(tiempo~operador+montaje)
anova(diseño2)

#INTERVALOS DE TUKEY PARA LAS DIFERENCIAS DE MEDIAS SEGÚN OPERADOR Y SEGÚN MONTAJE
TukeyHSD(diseño2,"operador",conf.level=0.95)
TukeyHSD(diseño2,"montaje",conf.level=0.95)

HSD.test(diseño2,"operador", group=TRUE,console=TRUE) #Comparaciones de Tukey

#GRÁFICOS DE INTERVALOS DE TUKEY
layout(cbind(c(1,1),c(2,2)))
plot(TukeyHSD(diseño2,"operador",conf.level = 0.95),cex.lab=0.8,las=1)
plot(TukeyHSD(diseño2,"montaje",conf.level = 0.95),cex.lab=0.8,las=1)

#OBTENIENDO EFECTOS DE TRATAMIENTOS, RESULTADOS PARA TEST DE SIGNIFICANCIA Y SUS I.C DEL 95%
efect.operador1=fit.contrast(diseño2,"operador",rbind(":efecto operador 1"=c(5/6,-1/6,-1/6,-1/6,-1/6,-1/6)),conf=0.95)
efect.operador2=fit.contrast(diseño2,"operador",rbind(":efecto operador 2"=c(-1/6,5/6,-1/6,-1/6,-1/6,-1/6)),conf=0.95)
efect.operador3=fit.contrast(diseño2,"operador",rbind(":efecto operador 3"=c(-1/6,-1/6,5/6,-1/6,-1/6,-1/6)),conf=0.95)
efect.operador4=fit.contrast(diseño2,"operador",rbind(":efecto operador 4"=c(-1/6,-1/6,-1/6,5/6,-1/6,-1/6)),conf=0.95)
efect.operador5=fit.contrast(diseño2,"operador",rbind(":efecto operador 5"=c(-1/6,-1/6,-1/6,-1/6,5/6,-1/6)),conf=0.95)
efect.operador6=fit.contrast(diseño2,"operador",rbind(":efecto operador 6"=c(-1/6,-1/6,-1/6,-1/6,-1/6,5/6)),conf=0.95)
rbind(efect.operador1,efect.operador2,efect.operador3,efect.operador4,efect.operador5,efect.operador6)

#OBTENIENDO TEST DE NORMALIDAD SOBRE RESIDUALES ESTUDENTIZADOS INTERNAMENTE
shapiro.test(rstandard(diseño2))


#OBTENIENDO DE GRÁFICOS PARA VALIDACIÓN DE SUPUESTOS CON RESIDUOS ESTUDENTIZADOS INTERNAMENTE
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
plot(fitted(diseño2),rstandard(diseño2),ylim=c(-2.5,2.5),cex=1.5,main="Residuales estudentizados vs. valores ajustados")
abline(h=c(-2,0,2),col=2)

stripchart(rstandard(diseño2)~montaje,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1.5,xlab="Montaje",main="Residuales estudentizados vs. montaje")
abline(h=c(-2,0,2),col=2)

stripchart(rstandard(diseño2)~operador,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1.5,xlab="Operador",main="Residuales estudentizados vs. operador")
abline(h=c(-2,0,2),col=2)

qqnorm(rstandard(diseño2),cex=1.5)
qqline(rstandard(diseño2),col=2,lty=2)
legend("topleft",legend=c("Shapiro-Wilk Test",expression(W==0.97285),expression(PValue== 0.8492)),cex=1.1)

detach(ensamble2)

