rm(list=ls(all=TRUE))
datos13=data.frame(Lote=factor(rep(1:5,times=5)),ContCalcio=scan()) 
23.46 23.59 23.51 23.28 23.29 
23.48 23.46 23.64 23.40 23.46
23.56 23.42 23.46 23.37 23.37
23.39 23.49 23.52 23.46 23.32
23.40 23.50 23.49 23.39 23.38

datos13
attach(datos13)
mediasLot=sapply(split(ContCalcio,Lote),mean);mediasLot
boxplot(ContCalcio~Lote)
lines(1:5,mediasLot,col=2,type="b",pch=19)


#AJUSTANDO EFECTOS ALEATORIOS PARA OBTENER COMPONENTES DE VARIANZA
diseño=lmer(ContCalcio~1|Lote)


#LO SIGUIENTE NOS DA ALGUNAS MEDIDAS DE AJUSTE Y LAS ESTIMACIONES DE LAS COMPONENTES DE VARIANZA
#OBSERVE QUE LO QUE APARECE EN LA SALIDA COMO INTERCEPTO ES SIMPLEMENTE EL VALOR DE LA MEDIA GLOBAL MUESTRAL
summary(diseño)

#INTERVALOS DE CONFIANZA ASINTÓTICOS PARA LAS RAÍCES CUADRADAS DE LAS VARIANZAS Y MEDIA GLOBAL
#sólo son buenas aproximaciones con grados de libertad mayores a 45, en cada componente de varianza!!
confint(diseño)

#OBTENIENDO LA ANOVA 
summary(aov(ContCalcio~Error(Lote)))
dis2=aov(ContCalcio~Error(Lote))
summary(dis2)

anova(aov(ContCalcio~Lote))
## se rechaza la hipotesis nula por tanto 

#OBTENEMOS RESIDUALES INTERNAMENTE ESTUDENTIZADOS Y VALORES AJUSTADOS COMO SI EL MODELO FUESE DE EFECTOS FIJOS Y CON ELLOS HACEMOS GRÁFICOS DE RESIDUALES
res.estudent=stdres(aov(ContCalcio~Lote))
Yhat=fitted(aov(ContCalcio~Lote))



#GRÁFICO DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS VS. VALORES AJUSTADOS
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
plot(Yhat,res.estudent,xlab="Respuesta ajustada",ylab="Residuales internamente estudentizados",pch=1:5,col=c(1:5),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("topright",legend=c(1:5),col=c(1:5),pch=c(1:5))

#GRÁFICO DE RESIDUOS VS. NIVELES FACTOR FARDO
stripchart(res.estudent~fardos,xlab="fardos",ylab="Residuales internamente estudentizados",vertical=T, pch=1:5,col=c(1:5),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("topright",legend=c(1:5),col=c(1:5),pch=c(1:5))

#NORMALIDAD SOBRE LOS ERRORES
shapiro.test(res.estudent)
qqnorm(res.estudent,main="Gráfico probabilidad normal\nsobre residuos internamente estudentizados")
qqline(res.estudent,lty=2)

#NORMALIDAD SOBRE LOS EFECTOS ALEATORIOS
#Calcular medias de tratamientos estandarizadas
medias.fardos.estand=scale(mediasLot)
medias.fardos.estand
qqnorm(medias.fardos.estand,main="Gráfico de probabilidad normal\npara los efectos")
qqline(medias.fardos.estand,lty=2,col=2)
abline(h=0,lty=2)
abline(v=0,lty=2)

detach(datos)
