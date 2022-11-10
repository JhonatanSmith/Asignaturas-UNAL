datos=data.frame(scan(what=list(Porc.Hidrocarb=0,Porc.PurezaO2=0))) #columna1 Porcentaje de hidrocarburos, columna2 Pureza O2
0.99 90.01
1.02 89.05
1.15 91.43
1.29 93.74
1.46 96.73
1.36 94.45
0.87 87.59
1.23 91.77
1.55 99.42
1.40 93.65
1.19 93.54
1.15 92.52
0.98 90.56
1.01 89.54
1.11 89.85
1.20 90.39
1.26 93.25
1.32 93.41
1.43 94.98
0.95 87.33


attach(datos)

#Ajustando MRLS
modelo=lm(Porc.PurezaO2~Porc.Hidrocarb)

library(car)
influence.measures(modelo)
influence.measures(modelo)$is.inf #para identificar por cuál criterio específico
                                  #se detecta anomalía
infIndexPlot(modelo)
influencePlot(modelo,ylim=c(-2.5,3),xlim=c(0,0.4)) #Ojo con estos limites 


plot(fitted(modelo),rstudent(modelo),ylim=c(-2.5,2.5),xlab="% pureza O2 estimada",
ylab="Residuales estudentizados externamente",cex=1.2)
abline(h=c(-2,0,2),col=2)

plot(Porc.Hidrocarb,rstudent(modelo),ylim=c(-2.5,2.5),cex=1.2,xlab="% hidrocarburos presentes",
ylab="Residuales estudentizados externamente")
abline(h=c(-2,0,2),col=2)

#Graficando los datos e identificando las obs. con problemas, en este caso la obs. 7 y 9
plot(Porc.Hidrocarb,Porc.PurezaO2,cex=1.2,bty="n",font=3)
points(Porc.Hidrocarb[c(7,9)],Porc.PurezaO2[c(7,9)],col=2,cex=1.2,pch=19)
text(Porc.Hidrocarb[c(7,9)],Porc.PurezaO2[c(7,9)],labels=c("Obs. 7","obs. 9"),pos=c(3,2))
lines(Porc.Hidrocarb,fitted(modelo),lwd=2,col=2)

detach(datos)