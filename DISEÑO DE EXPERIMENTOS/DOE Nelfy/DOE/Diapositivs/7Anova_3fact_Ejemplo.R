#LECTURA DE LOS DATOS
datos.tela=data.frame(trat.superf=factor(rep(c("S1","S1","S2","S2"),times=6)), 
                      sustancia=factor(rep(c("F1","F2","F1","F2"),times=6)), 
                      proporción=factor(c(rep("25%",8),rep("50%",8),
                                          rep("75%",8))),pérdida.peso=scan()) 
194 239 155 137
208 187 173 160
233 224 198 129
241 243 177 98
265 243 235 155
269 226 229 132
datos.tela
attach(datos.tela)
#INTERACCIÓN TRIPLE
win.graph(width=10,height=5)
layout(cbind(c(1),c(2)))
interaction.plot(proporción[sustancia=="F1"],trat.superf[sustancia=="F1"], 
                 pérdida.peso[sustancia=="F1"],type="b",pch=c(1,2),
                 col=c("black","red"),lwd=3,
                 main="Figura 1")
interaction.plot(proporción[sustancia=="F2"],trat.superf[sustancia=="F2"], 
                 pérdida.peso[sustancia=="F2"],type="b",pch=c(1,2),
                 col=c("black","red"),lwd=3,
                 main="Figura 2")
#INTERACCIONES DOBLES
layout(rbind(c(1,1,2,2),c(0,3,3,0))) 
interaction.plot(proporción,trat.superf,pérdida.peso,type="b",pch=c(1,2),
                                                      col=c("black","red"),
                 lwd=2,main="Figura 3") 
interaction.plot(proporción,sustancia,pérdida.peso,type="b",pch=c(1,2),
                 col=c("black","red"),
                 lwd=2,main="Figura 4")
interaction.plot(trat.superf,sustancia,pérdida.peso,type="b",pch=c(1,2),
                 col=c("black","red"),lwd=2,main="Figura 5")
#BOXPLOTS EFECTOS PRINCIPALES
layout(rbind(c(1,1,2,2),c(0,3,3,0))) 
boxplot(pérdida.peso~proporción,boxwex=0.4,ylab="pérdida de peso",
        xlab="proporción sustancia",
        col=c("bisque","blue","gray"),main="Figura 6")
boxplot(pérdida.peso~trat.superf,boxwex=0.4,ylab="pérdida de peso", 
        xlab="tratamiento superficie",col=c("bisque","blue"),main="Figura 7")
boxplot(pérdida.peso~sustancia,boxwex=0.4,
        ylab="pérdida de peso",xlab="sustancia",col=c("bisque","blue"),main="Figura 8")
#MODELO ANOVA CON LA INTERACCIÓN TRIPLE
modelo1=aov(pérdida.peso~proporción*trat.superf*sustancia) 
anova(modelo1)
#MODELO ANOVA SIN LA INTERACCIÓN TRIPLE
modelo2=aov(pérdida.peso~proporción*trat.superf+
              proporción*sustancia+trat.superf*sustancia) 
anova(modelo2)
#CORRIENDO MODELO SIN INTERACCIÓN TRIPLE Y SIN EFECTO DE INTERACCIÓN 
#PROPORCIÓN:TRATAMIENTO #SUPERFICIAL
modelo3=aov(pérdida.peso~proporción*sustancia+trat.superf*sustancia)
anova(modelo3)
#ANÁLISIS RESIDUOS ESTANDARIZADOS MODELO 2
layout(rbind(c(1,1,2,2),c(3,3,4,4))) 
plot(fitted(modelo3),rstandard(modelo3),
                                          cex=2,ylim=c(-2.1,2.1),main="Figura 9") 
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo3)~proporción,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="Proporción sustancia",main="Figura 10")
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo3)~trat.superf,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="Tratamiento de superficie",main="Figura 11")
abline(h=c(-2,0,2),col=2)
stripchart(rstandard(modelo3)~sustancia,cex=2,vertical=TRUE, ylim=c(-2.5,2.5),
           pch=1,xlab="sustancia",main="Figura 12")
abline(h=c(-2,0,2),col=2)
win.graph()
qqnorm(rstandard(modelo3),cex=2)
qqline(rstandard(modelo3),col=2)
shapiro.test(rstandard(modelo3))
detach(datos.tela)