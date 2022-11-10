library(daewr)
library(rsm)
library(pid)
library(FrF2)
rm(list=ls(all=TRUE))
#Datos en orden de corrida. Se ingresan directamente las variables Xj codificadas en lugar de X1, X2, X3, D 
problema3=data.frame(scan(what=list(Corrida=0,Y=0,X1=0,X2=0,X3=0,X4=0,PuntoCentral="")))
1 2515 0 0 0 0 si
2 2300 -1 1 -1 1 no
3 2610 -1 -1 1 -1 no
4 2625 -1 -1 1 1 no
5 2500 0 0 0 0 si
6 2400 -1 -1 -1 1 no
7 2390 1 -1 -1 1 no
8 2510 1 1 -1 -1 no
9 2410 1 -1 -1 -1 no
10 2520 1 1 -1 1 no
11 2625 1 -1 1 -1 no
12 2475 0 0 0 0 si
13 2315 -1 1 -1 -1 no
14 2400 -1 1 1 -1 no
15 2500 -1 1 1 1 no
16 2400 0 0 0 0 si
17 2750 1 1 1 -1 no
18 2400 -1 -1 -1 -1 no
19 2630 1 -1 1 1 no
20 2710 1 1 1 1 no
#######Análisis sólo con puntos factoriales (excluyendo los puntos centrales) ########################################################################## 
problema3f=problema3[problema3$PuntoCentral=="no",]
#Gráficos de interacción sólo con puntos factoriales

#problema3$tratamiento<-c("centro","bd","c","cd","centro","d","ad",
#                         "ab","a","abd","ac","centro",
#                         "b","bc","bcd","centro","abc",1,"acd","abcd")

attach(problema3f)

interaction.plot(X1,X2,Y,type="X2",pch=X3(1,2),col=X3("black","red"),
                        lwd=4, cex=2,cex.lab=1.5,legend=F,xlab="X1") 
  legend("bottomright",legend=X3("X2=-1","X2=+1"),col=1:2,pch=1:2,lty=X3(2,1),bty="n",cex=1)
interaction.plot(X1,X3,Y,type="X2",pch=X3(1,2),col=X3("black","red"),
                            lwd=4,cex=2,cex.lab=1.5,legend=F,xlab="X1") 
  legend("bottomright",legend=X3("X3=-1","X3=+1"),col=1:2,pch=1:2,lwd=2,lty=X3(2,1),bty="n",cex=1)
interaction.plot(X1,X4,Y,type="X2",pch=X3(1,2),col=X3("black","red"),
                            lwd=4,cex=2,cex.lab=1.5,legend=F,xlab="X1") 
  legend("bottomright",legend=X3("D=-1","D=+1"),col=1:2,pch=1:2,lwd=2,lty=X3(2,1),bty="n",cex=1)
interaction.plot(X2,X3,Y,type="X2",pch=X3(1,2),col=X3("black","red"),
                            lwd=4,cex=2,cex.lab=1.5,legend=F,xlab="X2") 
  legend("right",legend=X3("X3=-1","X3=+1"),col=1:2,pch=1:2,lwd=2,lty=X3(2,1),bty="n",cex=1)
interaction.plot(X2,X4,Y,type="X2",pch=X3(1,2),col=X3("black","red"),
                            lwd=4,cex=2,cex.lab=1.5,legend=F,xlab="X2") 
  legend("bottomleft",legend=X3("D=-1","D=+1"),col=1:2,pch=1:2,lwd=2,lty=X3(2,1),bty="n",cex=1)
interaction.plot(X3,X4,Y,type="X2",pch=X3(1,2),col=X3("black","red"),
                            lwd=4,cex=2,cex.lab=1.5,legend=F,xlab="X3") 
  legend("bottomright",legend=X3("D=-1","D=+1"),col=1:2,pch=1:2,lwd=2,lty=X3(2,1),bty="n",cex=1)

detach(problema3f)
  
#Modelo saturado sin usar puntos centrales 
modelo1f=lm(Y~X1*X2*X3*X4,data=problema3f) 
summary(modelo1f)
anova(modelo1f)
paretoPlot(modelo1f,negative=X3("Negativo","orange"),positive=X3("Positivo","blue"))
halfnormal(modelo1f,code=T,alpha=0.1,linelwd=2,linecol=2,err.points=T, pch.set = X3(19, 16, 8))

#Modelo reducido sin usar puntos centrales 
modelo1f=lm(Y~X1+X2+X3+X1*X2,data=problema3f) 
summary(modelo1f)

#Corra el modelo eliminando los efectos no significativos para comprobar #significancia de los que quedan. El modelo resulta
#recuerde que si se retiene una interacción doble, también debe retenerse #los efectos principales de los dos factores que intervienen en la interacción
#escriba X3ódigo R para estimación modelo final depurado con puntos factoriales
########################################
#punto 2 (Análisis con todos los datos)
########################################
#Después de identificar los efectos que deben quedar según análisis con puntos factoriales, ajuste ese
#modelo usando ahora X1 las variables Y, X1, X2, X3, X4 según los efectos X1 considerar y usando todos los datos 
#guardados en data.frame "problema3"

attach(problema3)


#Modelo saturado con puntos centrales 
modelo1f=lm(Y~X1*X2*X3*X4,data=problema3) 
summary(modelo1f)

#Modelo saturado con puntos centrales termino 2
modelo2f=lm(Y~X1*X2*X3*X4+I(X1^2),data=problema3) 
summary(modelo1f)

anova(modelo1f,modelo2f)

#Modelo 3 reducido con puntos centrales
modelo1f=lm(Y~X1+X2+X3+X1*X2,data=problema3) 
summary(modelo1f)
anova(modelo1f)

#Modelo reducido con puntos centrales termino 2
modelo2f=lm(Y~X1+X2+X3+X1*X2+I(X1^2),data=problema3) 
summary(modelo1f)

anova(modelo1f,modelo2f)
#superficies de rta estimada
persp(modelo1f,X2~X1,at=list(X3=-1),col="skyblue",contours=list(z="bottom"))
persp(modelo1f,X2~X1,at=list(X3=1),col="skyblue",contours=list(z="bottom"))

persp(modelo1f,X3~X2,at=list(X1=-1),col="skyblue",contours=list(z="bottom"))
persp(modelo1f,X3~X2,at=list(X1=1),col="skyblue",contours=list(z="bottom"))

persp(modelo1f,X3~X1,at=list(X2=-1),col="skyblue",contours=list(z="bottom"))
persp(modelo1f,X3~X1,at=list(X2=1),col="skyblue",contours=list(z="bottom"))

# Test de incorrelación (independecia)
Box.test(residuals(modelo1f), lag = 6, type = "Ljung-Box") # Se rechaza independencia
# gráficos de residuales

  stripchart(residuals(modelo1f)~ Corrida,vertical=TRUE,ylim=c(-100,100),pch=19,cex=1,
             xlab="Orden de corrida",
             ylab="residuales")
  abline(h=0,lty=2, col = "red") # residuales comunes vs orden de corrida
  #title(sub="(a)", adj=1, line=3, font=1, cex.sub = 1.4)
  # Residuales estandarizados
  stripchart(rstandard(modelo1f)~fitted(modelo1f),vertical=TRUE,
             ylim=c(-2.5,2.5),xlab="Ajustados",ylab="residuales",pch=19,cex=1)
  abline(h=c(-2,0,2),lty=2, col = "red")
  #title(sub="(b)", adj=1, line=3, font=1, cex.sub = 1.4)
  win.graph()
  stripchart(rstandard(modelo1f)~ X1,vertical=TRUE,ylim=c(-2.5,2.5),
             ylab="residuales",xlab="X1",pch=19,cex=1)
  abline(h=c(-2,0,2),lty=2, col = "red")
  #title(sub="(c)", adj=1, line=3, font=1, cex.sub = 1.4)
  win.graph()
  stripchart(rstandard(modelo1f)~ X2,vertical=TRUE,ylim=c(-2.5,2.5),xlab="X2",pch=1,cex=1)
  abline(h=c(-2,0,2),lty=2, col = "red")
  #title(sub="(d)", adj=1, line=3, font=1, cex.sub = 1.4)
  win.graph()
  stripchart(rstandard(modelo1f)~ X3,vertical=TRUE,ylim=c(-2.5,2.5),xlab="X3",pch=1,cex=1)
  abline(h=c(-2,0,2),lty=2, col = "red")
  title(sub="(e)", adj=1, line=3, font=1, cex.sub = 1.4)
  win.graph()
  stripchart(rstandard(modelo1f)~ X4,vertical=TRUE,ylim=c(-2.5,2.5),xlab="X4",pch=1,cex=1)
  abline(h=c(-2,0,2),lty=2, col = "red")
  #title(sub="(f)", adj=1, line=3, font=1, cex.sub = 1.4)



detach(problema3)


#########################Problema 6###############################################

PROBLEMA6=data.frame(scan(what=list(Corrida=0,A=0,B=0,C=0,D=0,E=0,F=0,Y1=0,Y2=0)))
9 -1 -1 -1 -1 -1 -1 6.2 4.86
5 +1 -1 -1 -1 +1 -1 5.6 4.86
6 -1 +1 -1 -1 +1 +1 5.8 4.85
1 +1 +1 -1 -1 -1 +1 5.8 4.99
14 -1 -1 +1 -1 +1 +1 5.7 4.94
10 +1 -1 +1 -1 -1 +1 6.4 4.74
13 -1 +1 +1 -1 -1 -1 6.4 4.83
12 +1 +1 +1 -1 +1 -1 6.6 4.85
11 -1 -1 -1 +1 -1 +1 5.3 4.81
3 +1 -1 -1 +1 +1 +1 6.6 4.81
15 -1 +1 -1 +1 +1 -1 5.2 4.98
16 +1 +1 -1 +1 -1 -1 5.5 4.98
8 -1 -1 +1 +1 +1 -1 6.9 4.84
4 +1 -1 +1 +1 -1 -1 7.1 4.85
2 -1 +1 +1 +1 -1 +1 6.7 4.96
7 +1 +1 +1 +1 +1 +1 6.9 4.84

# Series plot
library(dplyr)
library(lattice)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
p <- ggplot(PROBLEMA6, aes(x=Corrida, y=Y2)) +
  geom_line( color="#69b3a2")+theme_ipsum() + theme(axis.text.x=element_text(angle=60, hjust=1))
p + ylab("pH") + xlab("Numero de
corrida")+theme(axis.text=element_text(size=40),axis.title=element_text(size=14))
library(rsm);library(pid);library(daewr);library(FrF2)
diseno6=FrF2(nruns=16,nfactors=6,randomize=FALSE)
diseno6
design.info(diseno6)$aliased
PROBLEMA62 <- PROBLEMA6[order(PROBLEMA6$Corrida), ]
PROBLEMA62
modelo6=lm(Y2~A+B+C+D+E+F+A*B+A*C+A*D+A*E+A*F+B*D+B*F+A*B*D+A*C*D,data =PROBLEMA62)
summary(modelo6)
#Pareto plot
paretoPlot(modelo6,negative=c("Negativo","orange"),positive=c("Positivo","blue"))
halfnormal(modelo6,code=T,alpha=0.2,linelwd=2,linecol=2,pch.set = c(19, 16, 8),main="Gráfico para variable
pH con α= 0.2")
modelo62=lm(Y2~A+B+C+A*C+A*B*D,data = PROBLEMA62)
summary(modelo62)
mediasA=sapply(split(PROBLEMA62$Y2,PROBLEMA62$A),mean)
plot(c(-1,1),mediasA,type="l",lwd=6,xaxt="n",xlab="A:Levadura",ylab="Medias de pH",sub = "(a)",adj=1)
axis(1,at=c(-1,1),labels=c(-1,1))
mediasB=sapply(split(PROBLEMA62$Y2,PROBLEMA62$B),mean)
plot(c(-1,1),mediasB,type="l",lwd=6,xaxt="n",xlab="B:Sal",ylab="Medias de pH",sub = "(b)",adj=1)
axis(1,at=c(-1,1),labels=c(-1,1))
mediasC=sapply(split(PROBLEMA62$Y2,PROBLEMA62$C),mean)
plot(c(-1,1),mediasC,type="l",lwd=6,xaxt="n",xlab="C:Fosfato",ylab="Medias de pH",sub = "(c)",adj=1)
axis(1,at=c(-1,1),labels=c(-1,1))
mediasD=sapply(split(PROBLEMA62$Y2,PROBLEMA62$D),mean)
plot(c(-1,1),mediasD,type="l",lwd=6,xaxt="n",xlab="D:Sulfato",ylab="Medias de pH",sub = "(d)",adj=1)
axis(1,at=c(-1,1),labels=c(-1,1))
#interacciones
interaction.plot(PROBLEMA62$A,PROBLEMA62$B,PROBLEMA62$Y2,type="b",pch=c(1,2),col=c("black","red"),lwd=2,
                 ylab="Promedio de pH",xlab = "A:Levadura", trace.label = "B:Sal",main="Interacción A y B",sub = "(a)",adj=1)
interaction.plot(PROBLEMA62$A,PROBLEMA62$C,PROBLEMA62$Y2,type="b",pch=c(1,2),col=c("black","red"),lwd=2,
                 ylab="Promedio de pH",xlab = "A:Levadura", trace.label = "C:Fosfato",main="Interacción A y C",sub =
                   "(b)",adj=1)
interaction.plot(PROBLEMA62$A,PROBLEMA62$D,PROBLEMA62$Y2,type="b",pch=c(1,2),col=c("black","red"),lwd=2,
                 ylab="Promedio de pH",xlab = "A:Levadura", trace.label = "D:Sulfato",main="Interacción A y D",sub =
                   "(c)",adj=1)
interaction.plot(PROBLEMA62$B,PROBLEMA62$D,PROBLEMA62$Y2,type="b",pch=c(1,2),col=c("black","red"),lwd=2,
                 ylab="Promedio de pH",xlab = "B:Sal", trace.label = "D:Sulfato",main="Interacción B y D",sub = "(d)",adj=1)
library(dae)
data(SPLGrass.dat)
interaction.ABC.plot(Y2, x.factor=A,
                     groups.factor=B, trace.factor=D,
                     
                     data=PROBLEMA62,
                     ylab="Promedio de pH")

r = modelo62$residuals
Box.test(r, lag = 6, type = "Ljung-Box")
qqnorm(r)
qqline(r)

shapiro.test(rstandard(modelo62))
library(lmtest)
bptest(modelo62)
library(MASS)
residuos_1 <- stdres(modelo62)
ajustados_1 <- fitted(modelo62)
plot(fitted(modelo62),rstandard(modelo62),cex=2,ylim=c(-2.1,2.1),xlab = "Valores Ajustados", main = 'Errores
homocedásticos')
abline(h=c(-2,0,2),col=2)
plot(ajustados_1, abs(residuos_1), pch = 16, col = 'darkcyan',
     xlab = 'V. explicativa', ylab = 'abs(Residuos)', main = 'Errores homocedásticos')
lines(loess.smooth(ajustados_1, abs(residuos_1)), col = 'red3', lwd = 3, lty = 3)
stripchart(rstandard(modelo62)~PROBLEMA6$A,cex=2,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,
           xlab="A:Levadura",sub = "(a)",adj=1)
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo62)~PROBLEMA6$B,cex=2,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,
           xlab="B:Sal",sub = "(b)",adj=1)
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo62)~PROBLEMA6$C,cex=2,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,
           xlab="C:Fosfato",sub = "(c)",adj=1)
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo62)~PROBLEMA6$D,cex=2,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,
           xlab="D:Sulfato",sub = "(d)",adj=1)
abline(h=c(-2,0,2),col=2)


