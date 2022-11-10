#Programa R Problema 3.6.1 Relación lineal en un proceso de grabado de semiconductores (continuaci?n)
rm(list=ls(all=TRUE))
#Lectura de datos ingresando por teclado
#suponiendo que el registro está en orden de recolección
datos=data.frame(scan(what=list(Flujo.Cloro=0,Rapidez.Grabado=0))) #columna1 Flujo de cloro, columna2 Rapidez de grabado
1.5 23.0
2.0 25.0
2.5 33.5
3.5 40.5
4.0 49.0
1.5 24.5
2.5 30.0
3.0 40.0
3.5 47.0

datos

attach(datos)

#Ajustando MRLS
modelo=lm(Rapidez.Grabado~Flujo.Cloro)
anova(modelo) #Obteniendo ANOVA

summary(modelo) #Obteniendo tabla de parámetros ajustados


plot(Flujo.Cloro,Rapidez.Grabado,main="Gráfico de Dispersión\nProblema sobre proceso de grabado de semiconductores",xlab="Flujo de cloro (SCMM)",ylab="Rapidez de grabado (100A/min)",cex=1.5,bty="n",font=3,font.main=4) 
lines(loess.smooth(Flujo.Cloro,Rapidez.Grabado,family="gaussian",span=0.8),lty=1,lwd=2,col="blue")
lines(Flujo.Cloro,fitted(modelo),lty=1,col=2,lwd=2)
legend("topleft",legend=c("Curva de ajuste LOESS","Recta ajustada por RLS"),col=c(4,2),lwd=2,lty=c(1,1),bty="n")

#Serie de tiempo de residuos de ajuste
win.graph()
plot.ts(residuals(modelo),type="b",ylim=c(min(residuals(modelo),-2*summary(modelo)$sigma),max(residuals(modelo),2*summary(modelo)$sigma)),xlab="Orden de observación",ylab="Residuales",main="Residuales vs. orden de observación")
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),lty=2,col=2)

#Graficas residuales externamente estudentizados
win.graph()
plot(fitted(modelo),rstudent(modelo),ylim=c(min(rstudent(modelo),-2),max(rstudent(modelo),2)),xlab="Rapidez de grabado (100A/min)",ylab="Residuales externamente estudentizados",main="Residuales externamente estudentizados vs. valores ajustados")
abline(h=c(-2,0,2),lty=2,col=2)

win.graph()
plot(Flujo.Cloro,rstudent(modelo),ylim=c(min(rstudent(modelo),-2),max(rstudent(modelo),2)),xlab="Flujo de cloro (SCMM)",ylab="Residuales externamente estudentizados",main="Residuales externamente estudentizados vs. Predictor")
abline(h=c(-2,0,2),lty=2,col=2)


#Test Durbin-Watson
library(car)
#Para autocorrelaci?n de orden 1 positiva
durbinWatsonTest(modelo,alternative="positive")

#Para autocorrelaci?n de orden 1 negativa
durbinWatsonTest(modelo,alternative="negative")

test2=shapiro.test(rstudent(modelo)) #Test de normalidad sobre residuales externamente estudentizados 
win.graph()
#Gráfico de normalidad con informaci?n del test Shapiro
qqnorm(rstudent(modelo),cex=1.5,bty="n",font=3,font.main=3)
qqline(rstudent(modelo),lty=1,lwd=2,col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test2$statistic,test2$p.value),digits=4)),cex=0.8)

#Test de carencia de ajuste
library(rsm)
summary(rsm(Rapidez.Grabado~FO(Flujo.Cloro)))$lof

#o bien
anova(modelo,aov(Rapidez.Grabado~factor(Flujo.Cloro)))


plot(Flujo.Cloro,log(Rapidez.Grabado))    
detach(datos)
