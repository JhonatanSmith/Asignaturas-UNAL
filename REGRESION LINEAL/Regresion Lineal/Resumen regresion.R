Los pasos a seguir siempre son los siguientes:
  
  1) se gráfica dispersión
# plot(x,y) este codigo solo es un grafico de dispersion
plot(Hidrocarb,pureza02, main = "Grafico de dispersion\nProblema sobre proceso de destilacion",
     xlab = "% Hidrocarburos", ylab="% Puereza O2")

2) se aplica LOESS
#Curva loess
lines(loess.smooth(Hidrocarb,pureza02,family="gaussian",
                   span=0.8),lty=1,lwd=2,col="red")

3) Se verifica si se hace alguna transformación de ser necesaria. Graficamente 
se puede ver esto, si la curva LOESS tiene desviaciones o si no se ajustan bien a
todos los datos (dispersion extraña)

4) se aplica función lm() de R
modelo = lm(y~x) #Este es el modelo RLS del problema dado
summary(modelo) #Tabla de parametros del modelo
De aqui se pueden sacar valores como:
  B0 (intercepto)
  B1 (Cambio, pendiente)
  R^2: Explica que tanto explica el modelo la varianza de los datos
  Erros standard (Std.error)
  El valor P para verificar si hay una relacion estadistica entre las variables X e Y.
  Este es el ultimo paso a realizar puesto se requiere primero probar supuestos.

5) se gráfica los residuales con y, y-estimado 
#Graficas residuales 
win.graph()
plot(fitted(modelo),residuals(modelo),ylim=c(min(residuals(modelo),-2*summary(modelo)$sigma),max(residuals(modelo),
2*summary(modelo)$sigma)),xlab="% pureza O2 estimada",ylab="Residuales",main="Residuales vs. valores ajustados")
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),lty=2,col=2)

win.graph()
plot(Porc.Hidrocarb,residuals(modelo),ylim=c(min(residuals(modelo),-2*summary(modelo)$sigma),
max(residuals(modelo),2*summary(modelo)$sigma)),
xlab="% hidrocarburos presentes",ylab="Residuales",main="Residuales vs. Predictor")
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),lty=2,col=2)

6) se verifica si hay carencia de ajuste ayudado de los errores y
de la ANOVA (varianza constante y sin formas raras, distribuido aleatoriamente 
alrededor del cero) osea, no es lineal. 
#Una sospecha de la carencia de ajuste se ve en los graficos de los residuos, deberia de verse
#algun patron. Si esto sucede, es por carencia de ajuste.

La prueba:
anova(modelo,aov(y~factorx))
De aqui se saca el valor Pr(>F) que es el p-value a tratar. La hipotesis es tq si
p>alfa se acepta hipotesis nula y no hay carencia de ajuste.

library(rsm)
summary(rsm(y~FO(x)))$lof

7) se verifica varianza constante con el test de BF o BP. Es para verificar si hay 
homocedasticidad. 

Para Breusch Pagan es:
  library(car)
ncvTest(modelo,var.formula = ~x)

OJO: Este test requiere que los errores sean normales. BP

Para el test de BF, revise las notas es una vuelta de las medias.

8) se verifica que los errores sean independientes con ayuda del test 
Durbin Watson (OJO: Requiere tener orden temporal de los datos) 

win.graph()
plot.ts(residuals(modelo),type="b",ylim=c(min(residuals(modelo),-2*summary(modelo)$sigma),max(residuals(modelo),2*summary(modelo)$sigma)),xlab="Orden de observación",ylab="Residuales",main="Residuales vs. orden de observación")
abline(h=c(-2*summary(modelo)$sigma,0,2*summary(modelo)$sigma),lty=2,col=2)

#Graficas residuales externamente estudentizados
win.graph()
plot(fitted(modelo),rstudent(modelo2),ylim=c(min(rstudent(modelo2),-2),max(rstudent(modelo2),2)),xlab="Rapidez de grabado (100A/min)",ylab="Residuales externamente estudentizados",main="Residuales externamente estudentizados vs. valores ajustados")
abline(h=c(-2,0,2),lty=2,col=2)

win.graph()
plot(Flujo.Cloro,rstudent(modelo),ylim=c(min(rstudent(modelo2),-2),max(rstudent(modelo2),2)),xlab="Flujo de cloro (SCMM)",ylab="Residuales externamente estudentizados",main="Residuales externamente estudentizados vs. Predictor")
abline(h=c(-2,0,2),lty=2,col=2)


#Test Durbin-Watson
library(car)
#Para autocorrelación de orden 1 positiva
durbinWatsonTest(modelo,alternative="positive")

#Para autocorrelación de orden 1 negativa
durbinWatsonTest(modelo,alternative="negative")

#Para autocorrelación de orden 1
durbinWatsonTest(modelo,alternative="two.sided")


9) Se verifica normalidad en los supuestos de los errores. Prueba de Shapiro Wilk y tambien
Presencia de Outliers. Se recomienda residuos estudentizados.

test2=shapiro.test(rstudent(modelo)) #Test de normalidad sobre residuales externamente estudentizados 
win.graph()
#Gráfico de normalidad con información del test Shapiro
qqnorm(rstudent(modelo),cex=1.5,bty="n",font=3,font.main=3)
qqline(rstudent(modelo),lty=1,lwd=2,col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test2$statistic,test2$p.value),digits=4)),cex=0.8)

shapiro.test()

