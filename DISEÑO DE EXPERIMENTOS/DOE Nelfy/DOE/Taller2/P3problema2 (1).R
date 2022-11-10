

rm(list=ls(all=TRUE))
problema2=data.frame(A=factor(rep(c("Marca1","Marca2"),each=8)),C=factor(rep(c("T1","T2"),times=8)),B=factor(rep(rep(c("Aceite","Mantequilla"),each=2),times=4)),Y=scan())
0.44 0.02 0.39 0.07 
0.16 0.06 0.17 0.21 
0 0.21 0.36 0.62 
0.04 0.08 0.84 0.43 



attach(problema2)

#INTERACCIÓN TRIPLE
win.graph() 
interaction.plot(A[C=="T1"],B[C=="T1"],Y[C=="T1"],type="b",pch=c(1,2,3),
                 col=c("black","red","blue"),lwd=3,lty=c(1,2,3),
                             main="g")
win.graph() 
interaction.plot(A[C=="T2"],B[C=="T2"],Y[C=="T2"],type="b",pch=c(1,2,3),
                 col=c("black","red","blue"),lwd=3,,lty=c(1,2,3),
                             main=" Interacción A*B cuando C=4h")
#Complete aquí código R

#concentracion = A, presion=B, tiempo coc=C

#INTERACCIONES DOBLES
layout(rbind(c(1,1,2,2),c(0,3,3,0))) 
interaction.plot(A,C,Y,type="b",pch=c(1,2),
                 col=c("black","red"),
                 lwd=2,main="Figura 3") 
interaction.plot(A,B,Y,type="b",pch=c(1,2),
                 col=c("black","red"),
                 lwd=2,main="Figura 4")
interaction.plot(C,B,Y,type="b",pch=c(1,2),
                 col=c("black","red"),lwd=2,main="Figura 5")
#BOXPLOTS EFECTOS PRINCIPALES
layout(rbind(c(1,1,2,2),c(0,3,3,0))) 
boxplot(Y~A,boxwex=0.4,ylab="Concentración porcentual de fibra",
        xlab="A B",
        col=c("bisque","blue","gray"),main="Figura 6")
boxplot(Y~C,boxwex=0.4,ylab="Concentración porcentual de fibra", 
        xlab="tiempo de cocción de la pulpa ",col=c("bisque","blue"),main="Figura 7")
boxplot(Y~B,boxwex=0.4,
        ylab="Concentración porcentual de fibra",xlab="B",col=c("bisque","blue"),main="Figura 8")
#MODELO ANOVA CON LA INTERACCIÓN TRIPLE
modelo1=aov(Y~A*C*B) 
anova(modelo1)
#MODELO ANOVA SIN LA INTERACCIÓN TRIPLE
modelo2=aov(Y~A*C+A*B+C*B) 
anova(modelo2)
#CORRIENDO MODELO SIN INTERACCIÓN TRIPLE Y SIN EFECTO DE INTERACCIÓN 
#A:TRATAMIENTO #SUPERFICIAL
modelo3=aov(Y~A*B+C*B)
anova(modelo3)
#ANÁLISIS RESIDUOS ESTANDARIZADOS MODELO 2
#layout(rbind(c(1,1,2,2),c(3,3,4,4))) 
plot(fitted(modelo3),rstandard(modelo3),
     cex=2,ylim=c(-2.1,2.1),main="Figura 9") 
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo3)~A,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="A B",main="Figura 10")
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo3)~C,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="Tratamiento de superficie",main="Figura 11")
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo3)~B,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="B",main="Figura 12")
abline(h=c(-2,0,2),col=2)
win.graph()
qqnorm(rstandard(modelo3),cex=2)
qqline(rstandard(modelo3),col=2)
shapiro.test(rstandard(modelo3))

summary(lsmeans(modelo2,~A*B*C))



detach(problema2)

