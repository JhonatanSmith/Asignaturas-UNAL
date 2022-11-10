library(daewr)
library(lme4)
library(MASS)

impresion=data.frame(temperatura=factor(rep(c("A1","A2","A3","A4"),each=12)),tinta=factor(rep(c("B1","B2","B3"),times=16)),nitidez=scan())
252.23 296.20 288.59
253.32 327.97 299.12
239.63 295.67 283.26
247.48 328.08 279.47
241.72 261.04 258.29
256.31 209.03 212.89
241.85 261.59 233.84
257.55 240.16 247.95
88.95  204.56 183.43
90.48  172.36 161.39
67.24  240.02 200.66
96.44  208.66 189.55
233.15 329.03 242.48
236.80 316.88 232.05
251.28 335.31 253.80
240.96 332.46 223.89

impresion

attach(impresion)
#GRÁFICOS PARA ANALIZAR LOS DATOS EXPERIMENTALES
mediastemperatura=sapply(split(nitidez,temperatura),mean)
mediastinta=sapply(split(nitidez,tinta),mean)
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
boxplot(nitidez~temperatura,xlab="Temperatura",boxwex=0.4,col=c("bisque","blue","gray","orange"))
lines(1:4,mediastemperatura,type="b",pch=19,col=2,lty=2,lwd=2)
boxplot(nitidez~tinta,xlab="Tinta",boxwex=0.4,col=c("bisque","blue","gray"))
lines(1:3,mediastinta,type="b",pch=19,col=2,lty=2,lwd=2)
interaction.plot(temperatura,tinta,nitidez,type="b",lwd=2,col=1:3,pch=1:3)
interaction.plot(tinta,temperatura,nitidez,type="b",lwd=2,col=1:4,pch=1:4)

#sumas de cuadrados y cuadrado medios
modelo=aov(nitidez~Error(temperatura*tinta))
summary(modelo)

#componentes de varianza
modelob=lmer(nitidez~1+(1|temperatura)+(1|tinta)+(1|temperatura:tinta))
summary(modelob)


Anovafactorialaleatoriosconinteraccion=function(modelo){
dfA=anova(modelo)[1][1,]
dfB=anova(modelo)[1][2,]
dfi=anova(modelo)[1][3,]
dfe=anova(modelo)[1][4,]
SSA=anova(modelo)[2][1,]
SSB=anova(modelo)[2][2,]
SSAB=anova(modelo)[2][3,]
SSE=anova(modelo)[2][4,]
MSA=anova(modelo)[3][1,]
MSB=anova(modelo)[3][2,]
MSAB=anova(modelo)[3][3,]
MSE=anova(modelo)[3][4,]
F1=round(MSA/MSAB,digits=2)
F2=round(MSB/MSAB,digits=2)
F3=round(MSAB/MSE,digits=2)
vp1=round(pf(F1,df1=dfA,df2=dfi,lower.tail=F),digits=5)
vp2=round(pf(F2,df1=dfB,df2=dfi,lower.tail=F),digits=5)
vp3=round(pf(F3,df1=dfi,df2=dfe,lower.tail=F),digits=5)
nombres=attr(attr(modelo$model,"terms"),"term.labels")
anova1=data.frame(Source=nombres,DF=c(dfA,dfB,dfi),"Sum\ Sq"=c(SSA,SSB,SSAB),"Mean Sq"=c(MSA,MSB,MSAB),"F\ Value"=c(F1,F2," "),"P\ value"=c(vp1,vp2," "))
anova2=data.frame(Source=c(nombres[3],"Error"),DF=c(dfi,dfe),"Sum\ Sq"=c(SSAB,SSE),"Mean Sq"=c(MSAB,MSE),"F\ Value"=c(F3," "),"P\ value"=c(vp3," "))
cat("Tests of Hypotheses for Random Model Analysis of Variance","\n")
cat("Dependent Variable:",names(modelo$model)[1],"\n")
print(anova1)
cat("\n")
print(anova2)
}

modeloaux=aov(nitidez~temperatura*tinta) #correr modelo como si fuese efectos fijos
Anovafactorialaleatoriosconinteraccion(modelo=modeloaux)

shapiro.test(rstandard(modeloaux))
#OBTENIENDO GRÁFICOS DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS,
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
stripchart(rstandard(modeloaux)~temperatura,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="temperatura")
abline(h=c(-2,0,2),lty=2)
stripchart(rstandard(modeloaux)~tinta,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="tinta")
abline(h=c(-2,0,2),lty=2)
plot(fitted(modeloaux),rstandard(modeloaux),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(modeloaux))
qqline(rstandard(modeloaux),lty=2)
legend("topleft",legend=c("Shapiro-Wilk Test",expression(W==0.97884),expression(PValue== 0.5312)),cex=1.1)

detach(impresion)
