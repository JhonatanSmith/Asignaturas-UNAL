library(gmodels)
library(multcomp)
library(daewr)
library(car)
library(lsmeans)
library(agricolae)

reparación=data.frame(marca=factor(rep(1:3,times=15)),
técnico=factor(rep(c(1:3),each=15)),tiempo=scan())
62 57 59
48 45 53
63 39 67
57 54 66
69 44 47
51 61 55
57 58 58
45 70 50
50 66 69
39 51 49
59 58 47
65 63 56
55 70 51
52 53 44
70 60 50

reparación 

attach(reparación)

#GRÁFICOS PARA ANALIZAR LOS DATOS EXPERIMENTALES
mediastécnico=sapply(split(tiempo,técnico),mean)
mediasmarca=sapply(split(tiempo,marca),mean)

#Gráficos de interacción
interaction.plot(técnico,marca,tiempo,type="b",pch=c(1,2,3),col=c("black","blue","red"),lwd=2)

interaction.plot(marca,técnico,tiempo,type="b",pch=c(1,2,3),col=c("black","blue","red"),lwd=2)

boxplot(tiempo~técnico,xlab="técnico",ylab="tiempo",boxwex=0.4,col=c("bisque","blue","gray"))
lines(1:3,mediastécnico,type="b",pch=19,col=2,lty=2,lwd=2)

boxplot(tiempo~marca,xlab="marca",ylab="tiempo",boxwex=0.4,col=c("bisque","blue","gray"))
lines(1:3,mediasmarca,type="b",pch=19,col=2,lty=2,lwd=2)

#BOXPLOTS POR TRATAMIENTOS
boxplot(tiempo~técnico:marca,xlab="técnico*marca",col = c(rep("bisque",3),rep("blue",3),rep("gray",3)))
boxplot(tiempo~marca:técnico,xlab="marca*técnico",col = c(rep("bisque",3),rep("blue",3),rep("gray",3)))

#AJUSTE MODELO ANOVA CON INTERACCIÓN Y CÁCULO TABLA ANOVA
modelo=aov(tiempo~técnico*marca) 
anova(modelo) 
#MEDIAS DE TRATAMIENTOS CON I.C DEL 95%
summary(lsmeans(modelo,~técnico*marca))

#COMPARACIONES MÚLTIPLES DE TRATAMIENTOS POR TUKEY
#PERO NO ES MUY ÚTIL CON MUCHOS TRATAMIENTOS
TukeyHSD(modelo,"técnico:marca",conf.level=0.95)
plot(TukeyHSD(modelo,"técnico:marca",conf.level=0.95),cex.axis=0.8,las=1)

#FUNCIÓN DE USUARIO PARA EVALUAR IGUALDAD DE MEDIAS DE TRATAMIENTOS mu_ij
#EN CADA NIVEL DE LOS FACTORES. SUPONE UN DCA BALANCEADO:
#argumento modelo es para especificar el objeto R en que previamente
#se guardó el ajuste del modelo anova con función aov
#nreplicas se refiere al número de replicaciones de cada tratamiento

Comparmediasslices2=function(modelo,nreplicas){
respuesta=modelo$model[,1]
factor1=modelo$model[,2]
factor2=modelo$model[,3]
df1=length(levels(factor1))-1
df2=length(levels(factor2))-1
dfe=anova(modelo)[1][4,]
mediastrat=summary(lsmeans(aov(respuesta~factor1*factor2),~factor1*factor2))
MS2.1=nreplicas*sapply(split(mediastrat$lsmean,mediastrat$factor1),var)
MS1.2=nreplicas*sapply(split(mediastrat$lsmean,mediastrat$factor2),var)
SS2.1=df2*MS2.1
SS1.2=df1*MS1.2
MSE=anova(modelo)[3][4,]
F02.1=MS2.1/MSE
F01.2=MS1.2/MSE
pvalue2.1=pf(F02.1,df1=df2,df2=dfe,lower.tail=F)
pvalue1.2=pf(F01.2,df1=df1,df2=dfe,lower.tail=F)

slicebyfactor2.1=data.frame(niveles=modelo$xlevels[1],Df=df2,"Sum\ Sq"=SS2.1,"Mean Sq"=MS2.1,F0=F02.1,"P value"=pvalue2.1)
slicebyfactor1.2=data.frame(niveles=modelo$xlevels[2],Df=df1,"Sum\ Sq"=SS1.2,"Mean Sq"=MS1.2,F0=F01.2,"P value"=pvalue1.2)
cat("Significancia Efectos",names(modelo$model)[2],"*",names(modelo$model)[3],"sobre respuesta",names(modelo$model)[1],"en cada nivel de",names(modelo$xlevels)[1],"\n")
print(slicebyfactor2.1)
cat("\n")
cat("Significancia Efectos",names(modelo$model)[2],"*",names(modelo$model)[3],"sobre respuesta",names(modelo$model)[1],"en cada nivel de",names(modelo$xlevels)[2],"\n")
print(slicebyfactor1.2)
}

#Usando la función creada sobre el modelo anova ajustado previamente
Comparmediasslices2(modelo=modelo,nreplicas=5)

shapiro.test(rstandard(modelo))
#OBTENIENDO GRÁFICOS DE RESIDUOS ESTUDENTIZADOS INTERNAMENTE
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
stripchart(rstandard(modelo)~técnico,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="técnico")
abline(h=c(-2,0,2),lty=2)
stripchart(rstandard(modelo)~marca,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="marca")
abline(h=c(-2,0,2),lty=2)
plot(fitted(modelo),rstandard(modelo),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(modelo))
qqline(rstandard(modelo),lty=2)
legend("topleft",legend=c("Shapiro-Wilk Test",expression(W==0.96922),expression(PValue== 0.2712)),cex=1.1)

detach(reparación)




