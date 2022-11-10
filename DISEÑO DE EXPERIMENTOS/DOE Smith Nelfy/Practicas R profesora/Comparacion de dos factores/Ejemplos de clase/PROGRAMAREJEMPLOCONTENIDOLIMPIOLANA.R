rm(list=ls(all=TRUE))
library(lme4)
library(MASS)

datos=data.frame(fardos=factor(rep(1:7,times=4)),contl=scan())
52.33 56.99 54.64 54.90 59.89 57.76 60.27
56.26 58.69 57.48 60.08 57.76 59.68 60.30
62.86 58.20 59.29 58.72 60.26 59.58 61.09
50.46 57.35 57.51 55.61 57.53 58.08 61.45

datos

attach(datos)
#boxplots por niveles del factor

medias.fardos=sapply(split(contl,fardos),mean)
plot(contl~fardos,boxwex=0.5)
lines(1:7,medias.fardos,col=2,lty=2,type="b",pch=19)

#AJUSTANDO EFECTOS ALEATORIOS PARA OBTENER COMPONENTES DE VARIANZA
diseño=lmer(contl~1|fardos)

#LO SIGUIENTE NOS DA ALGUNAS MEDIDAS DE AJUSTE Y LAS ESTIMACIONES DE LAS COMPONENTES DE VARIANZA
#OBSERVE QUE LO QUE APARECE EN LA SALIDA COMO INTERCEPTO ES SIMPLEMENTE EL VALOR DE LA MEDIA GLOBAL MUESTRAL
summary(diseño)

#INTERVALOS DE CONFIANZA ASINTÓTICOS PARA LAS RAÍCES CUADRADAS DE LAS VARIANZAS Y MEDIA GLOBAL
#sólo son buenas aproximaciones con grados de libertad mayores a 45, en cada componente de varianza!!
confint(diseño)

#OBTENIENDO LA ANOVA 
summary(aov(contl~Error(fardos)))
dis2=aov(contl~Error(fardos))
summary(dis2)

anova(aov(contl~fardos))

#OBTENEMOS RESIDUALES INTERNAMENTE ESTUDENTIZADOS Y VALORES AJUSTADOS COMO SI EL MODELO FUESE DE EFECTOS FIJOS Y CON ELLOS HACEMOS GRÁFICOS DE RESIDUALES
res.estudent=stdres(aov(contl~fardos))
Yhat=fitted(aov(contl~fardos))

#GRÁFICO DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS VS. VALORES AJUSTADOS
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
plot(Yhat,res.estudent,xlab="Respuesta ajustada",ylab="Residuales internamente estudentizados",pch=1:7,col=c(1:6,"brown"),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("topright",legend=c(1:7),col=c(1:6,"brown"),pch=c(1:7))

#GRÁFICO DE RESIDUOS VS. NIVELES FACTOR FARDO
stripchart(res.estudent~fardos,xlab="fardos",ylab="Residuales internamente estudentizados",vertical=T, pch=1:7,col=c(1:6,"brown"),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("topright",legend=c(1:7),col=c(1:6,"brown"),pch=c(1:7))

#NORMALIDAD SOBRE LOS ERRORES
shapiro.test(res.estudent)
qqnorm(res.estudent,main="Gráfico probabilidad normal\nsobre residuos internamente estudentizados")
qqline(res.estudent,lty=2)

#NORMALIDAD SOBRE LOS EFECTOS ALEATORIOS
#Calcular medias de tratamientos estandarizadas
medias.fardos.estand=scale(medias.fardos)
medias.fardos.estand
qqnorm(medias.fardos.estand,main="Gráfico de probabilidad normal\npara los efectos")
qqline(medias.fardos.estand,lty=2,col=2)
abline(h=0,lty=2)
abline(v=0,lty=2)

detach(datos)


