library(car)
datos=data.frame(Seccion=factor(rep(c("A","B","C"),each=5)),
                 scan(what=list(Publicidad=0,Ventas=0)))
5.2 9
5.9 10
7.7 12
7.9 12
9.4 14
8.2 13
9.0 13
9.1 12
10.5 13
10.5 14
10.0 18
10.3 19
12.1 20
12.7 21
13.6 22

attach(datos)

#CAMBIANDO NIVEL DE REFERENCIA DE LA VARIABLE CUALITATIVA PARA LA SECCIoN C###
Seccion=relevel(Seccion,ref="C")
Seccion #observe que se informa que los niveles en su orden son C, A, B

#Gr?fico de dispersi?n con identificaci?n de Secciones
plot(Publicidad,Ventas,pch=as.numeric(Seccion),col=as.numeric(Seccion),
     xlab="Publicidad (cientos de d?lares)",
     ylab="Ventas (miles de d?lares)",cex=2,cex.lab=1.5)
legend("topleft",legend=c("C","A","B"),pch=c(1:3),col=c(1:3),cex=1.5)

##MODELO GRAL: RECTAS DIFERENTES. NIVEL DE REF=C###
modelo1=lm(Ventas~Publicidad*Seccion)
summary(modelo1)
confint(modelo1)

#AN?LISIS RESIDUALES EN modelo1
#PROBANDO HIP?TESIS LINEALES PARA MODELO AJUSTADO EN modelo1
names(coef(modelo1)) #Observe el nombre de los t?rminos en el modelo1
linearHypothesis(modelo1,"Publicidad:SeccionA=Publicidad:SeccionB")
linearHypothesis(modelo1,c("SeccionA=0","SeccionB=0","Publicidad:SeccionA=0",
                           "Publicidad:SeccionB=0"))
linearHypothesis(modelo1,c("Publicidad:SeccionA=0","Publicidad:SeccionB=0"))
