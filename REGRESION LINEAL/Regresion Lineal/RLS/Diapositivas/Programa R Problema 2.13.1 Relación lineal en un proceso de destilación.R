#Programa R Problema 2.13.1 Relación lineal en un proceso de destilación
rm(list=ls(all=TRUE))
#Lectura de datos ingresando por teclado
datos=data.frame(scan(what=list(A=0,B=0)))
1.5 5.3
2 6.8
3 11.2
4 18.4

datos #dé un vistazo para verificar que se leyó correctamente los datos

attach(datos) #Disponibilizando las variables guardadas en data.frame datos

plot(Porc.Hidrocarb,Porc.PurezaO2,main="Gráfico de Dispersión\nProblema sobre proceso de destilación",xlab="% hidrocarburos presentes",ylab="% pureza O2",cex=1.5,bty="n",font=3,font.main=4) 
lines(loess.smooth(Porc.Hidrocarb,Porc.PurezaO2,family="gaussian",span=0.8),lty=1,lwd=2,col="red")
legend("topleft",legend="Curva de ajuste LOESS",col=2,lwd=2,bty="n")


y = datos$B
x = datos$
lnY=log(y)
lnX=log(x)
#Ajustando MRLS
modelo=lm(lnY~lnX)
summary(modelo) #Obteniendo tabla de parámetros ajustados

plot(Porc.Hidrocarb,Porc.PurezaO2,main="Gráfico de Dispersión\nProblema sobre proceso de destilación",xlab="% hidrocarburos presentes",ylab="% pureza O2",cex=1.5,bty="n",font=3,font.main=4) 
lines(loess.smooth(Porc.Hidrocarb,Porc.PurezaO2,family="gaussian",span=0.8),lty=1,lwd=2,col="red")
lines(Porc.Hidrocarb,fitted(modelo),lty=1,col="blue",lwd=2)
legend("topleft",legend=c("Curva de ajuste LOESS","ajuste RLS"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2),bty="n")



anova(modelo) #Obteniendo ANOVA

confint(modelo,level=0.95) #intervalo de confianza para parámetros

#Graficas residuales 
win.graph()
plot(fitted(modelo),residuals(modelo),ylim=c(min(residuals(modelo),-2*summary(modelo)$sigma),max(residuals(modelo),2*summary(modelo)$sigma)),xlab="% pureza O2 estimada",ylab="Residuales",main="Residuales vs. valores ajustados")
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),lty=2,col=2)

win.graph()
plot(Porc.Hidrocarb,residuals(modelo),ylim=c(min(residuals(modelo),-2*summary(modelo)$sigma),max(residuals(modelo),2*summary(modelo)$sigma)),xlab="% hidrocarburos presentes",ylab="Residuales",main="Residuales vs. Predictor")
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),lty=2,col=2)

vcov(modelo)

test=shapiro.test(residuals(modelo)) #Test de normalidad sobre residuales
win.graph()
#Gráfico de normalidad con información del test Shapiro
qqnorm(residuals(modelo),cex=1.5,bty="n",font=3,font.main=3)
qqline(residuals(modelo),lty=1,lwd=2,col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test$statistic,test$p.value),digits=4)),cex=0.8)

#Gráfico de dispersión con recta ajustada e intervalos de confianza para la respuesta media y de predicción para la respuesta
library(HH) #Descargar librería HH previamente
win.graph(width=8,height=6)
ci.plot(modelo,xlab="% hidrocarburos presentes",ylab="% pureza O2",lty=c(2,1,3),cex=1.5,conf.level=.95)

#Para obtener predicciones en X=0.80, 0.92, I.C y I.P del 95%
#Respuesta media estimada  e I.C del 95% 
predict(modelo,newdata=data.frame(Porc.Hidrocarb=c(0.8,0.92)),level=0.95,interval="confidence") 

#Predicciones e I.P del 95%
predict(modelo,newdata=data.frame(Porc.Hidrocarb=c(0.8,0.92)),interval="prediction",level=0.95) 


detach(datos)