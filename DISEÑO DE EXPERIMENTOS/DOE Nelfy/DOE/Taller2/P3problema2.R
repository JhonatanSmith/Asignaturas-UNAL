rm(list=ls(all=TRUE)) 

problema2=data.frame(A=factor(rep(c(2,4,8),each=12)),
                     B=factor(rep(c(400,500,650),times=12)),
                     C=factor(rep(rep(c(3,4),each=3),times=6)),Y=scan()) 
196.6 197.7 199.8 198.4 199.6 200.6
196.0 196.0 199.4 198.6 200.4 200.9
198.5 196.0 198.4 197.5 198.7 199.6
197.2 196.9 197.6 198.1 198.0 199.0
197.5 195.6 197.4 197.6 197.0 198.5
196.6 196.2 198.1 198.4 197.8 199.8

attach(problema2)

#INTERACCIÓN TRIPLE
win.graph() 
interaction.plot(A[C==3],B[C==3],Y[C==3],type="b",pch=c(1,2,3),
                 col=c("black","red","blue"),lwd=3,lty=c(1,2,3),
                             main="Interacción A*B cuando C=3h")
win.graph() 
interaction.plot(A[C==4],B[C==4],Y[C==4],type="b",pch=c(1,2,3),
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
#=========================================================================

#ANÁLISIS RESIDUOS ESTANDARIZADOS MODELO 2
#layout(rbind(c(1,1,2,2),c(3,3,4,4))) 
plot(fitted(modelo2),rstandard(modelo2),
     cex=2,ylim=c(-2.1,2.1),main="Figura 9") 
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo2)~A,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="A",main="Figura 10")
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo2)~C,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="C",main="Figura 11")
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo2)~B,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="B",main="Figura 12")
abline(h=c(-2,0,2),col=2)
win.graph()
qqnorm(rstandard(modelo2),cex=2)
qqline(rstandard(modelo2),col=2)
shapiro.test(rstandard(modelo2))

detach(problema2)