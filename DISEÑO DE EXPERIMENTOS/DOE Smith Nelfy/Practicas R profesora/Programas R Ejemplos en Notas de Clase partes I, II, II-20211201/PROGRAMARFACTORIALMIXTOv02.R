library(daewr)
library(lme4)
library(MASS)
library(lmerTest)

datos=data.frame(VariedadTrigo=factor(rep(c("A","B","C","D"),times=12)),Fertilizante=factor(rep(1:3,each=16)),cosecha=scan())
35 45 24 55
26 39 23 48
38 39 36 39
20 43 29 49
55 64 58 68
44 57 74 61
68 62 49 60
64 61 69 75
97 93 89 82
89 91 98 78
92 82 85 89
99 98 87 92


datos

attach(datos)
#GRÁFICOS PARA ANALIZAR LOS DATOS EXPERIMENTALES
mediasvariedad=sapply(split(cosecha,VariedadTrigo),mean)
mediasfertilizante=sapply(split(cosecha,Fertilizante),mean)
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
boxplot(cosecha~VariedadTrigo,xlab="Variedad de trigo",boxwex=0.4,col=c("bisque","blue","gray","orange"))
lines(1:4,mediasvariedad,type="b",pch=19,col=2,lty=2,lwd=2)
boxplot(cosecha~Fertilizante,xlab="Fertilizante",boxwex=0.4,col=c("bisque","blue","gray"))
lines(1:3,mediasfertilizante,type="b",pch=19,col=2,lty=2,lwd=2)
interaction.plot(VariedadTrigo,Fertilizante,cosecha,type="b",lwd=2,col=1:3,pch=1:3)
interaction.plot(Fertilizante,VariedadTrigo,cosecha,type="b",lwd=2,col=1:4,pch=1:4)

Anovafactorialmixtoconinteraccion=function(modelo,norestringido="TRUE"){
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
if(norestringido=="TRUE"){
F1=round(MSA/MSAB,digits=2)
F2=round(MSB/MSAB,digits=2)
F3=round(MSAB/MSE,digits=2)
vp1=round(pf(F1,df1=dfA,df2=dfi,lower.tail=F),digits=5)
vp2=round(pf(F2,df1=dfB,df2=dfi,lower.tail=F),digits=5)
vp3=round(pf(F3,df1=dfi,df2=dfe,lower.tail=F),digits=5)
nombres=attr(attr(modelo$model,"terms"),"term.labels")
anova1=data.frame(Source=nombres,DF=c(dfA,dfB,dfi),"Sum\ Sq"=c(SSA,SSB,SSAB),"Mean Sq"=c(MSA,MSB,MSAB),"F\ Value"=c(F1,F2," "),"P\ value"=c(vp1,vp2," "))
anova2=data.frame(Source=c(nombres[3],"Error"),DF=c(dfi,dfe),"Sum\ Sq"=c(SSAB,SSE),"Mean Sq"=c(MSAB,MSE),"F\ Value"=c(F3," "),"P\ value"=c(vp3," "))
cat("The Unrestricted Mixed Model Analysis of Variance","\n")
cat("Dependent Variable:",names(modelo$model)[1],"\n")
print(anova1)
cat("\n")
print(anova2)
}
if(norestringido=="FALSE"){
F1=round(MSA/MSAB,digits=2)
F2=round(MSB/MSE,digits=2)
F3=round(MSAB/MSE,digits=2)
vp1=round(pf(F1,df1=dfA,df2=dfi,lower.tail=F),digits=5)
vp2=round(pf(F2,df1=dfB,df2=dfe,lower.tail=F),digits=5)
vp3=round(pf(F3,df1=dfi,df2=dfe,lower.tail=F),digits=5)
nombres=attr(attr(modelo$model,"terms"),"term.labels")
anova1=data.frame(Source=nombres[c(1,3)],DF=c(dfA,dfi),"Sum\ Sq"=c(SSA,SSAB),"Mean Sq"=c(MSA,MSAB),"F\ Value"=c(F1," "),"P\ value"=c(vp1," "))
anova2=data.frame(Source=c(nombres[2:3],"Error"),DF=c(dfB,dfi,dfe),"Sum\ Sq"=c(SSB,SSAB,SSE),"Mean Sq"=c(MSB,MSAB,MSE),"F\ Value"=c(F2,F3," "),"P\ value"=c(vp2,vp3," "))
cat("The Restricted Mixed Model Analysis of Variance","\n")
cat("Dependent Variable:",names(modelo$model)[1],"\n")
print(anova1)
cat("\n")
print(anova2)
}
}

#correr modelo como de efectos fijos pero colocar de primero el factor que realmente es fijo
modeloaux=aov(cosecha~VariedadTrigo*Fertilizante) 
Anovafactorialmixtoconinteraccion(modelo=modeloaux) #ANOVA mixto no restringido

Anovafactorialmixtoconinteraccion(modelo=modeloaux,norestringido="FALSE") #ANOVA mixto restringido

#Validación de supuestos usando residuos internamente estudentizados del modelo factorial
#como si fuese de efectos fijos
shapiro.test(rstandard(modeloaux))
#OBTENIENDO GRÁFICOS DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS,
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
stripchart(rstandard(modeloaux)~VariedadTrigo,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="Variedad de trigo")
abline(h=c(-2,0,2),lty=2)
stripchart(rstandard(modeloaux)~Fertilizante,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="Fertilizante")
abline(h=c(-2,0,2),lty=2)
plot(fitted(modeloaux),rstandard(modeloaux),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(modeloaux))
qqline(rstandard(modeloaux),lty=2)
legend("topleft",legend=c("Shapiro-Wilk Test",expression(W==0.97944),expression(PValue== 0.5554)),cex=1.1)


#modelo=aov(cosecha~VariedadTrigo+Error(Fertilizante+VariedadTrigo:Fertilizante))
#summary(modelo)


#Estimación de efectos y componentes de varianza en modelo mixto no restringido
modelob=lmer(cosecha~1+VariedadTrigo+(1|Fertilizante)+(1|VariedadTrigo:Fertilizante))
summary(modelob) #tomar sólo lo referente a Random effects:


#Estimación de efectos y componentes de varianza en modelo mixto sin interacción
modeloc=lmer(cosecha~1+VariedadTrigo+(1|Fertilizante))
summary(modeloc)


#Función usuario para construcción de los I.C para medias del factor de efectos fijos
#En modelo mixto con interacción
#El argumento modelo hace referencia al modelo ajustado con la función lmer
mediasfactorialmixtointerv02=function(modelo,alpha=0.05){
medias=lsmeansLT(modelo)
df=round(medias$df)
tcrit=qt(alpha,df=df,lower.tail=F)
lower=medias$Estimate-tcrit*medias$"Std. Error"
upper=medias$Estimate+tcrit*medias$"Std. Error"
mediasf=data.frame(medias$Estimate,medias$"Std. Error",medias$df,medias$"t value",lower,upper,row.names=dimnames(medias)[[1]])
names(mediasf)=dimnames(medias)[[2]][1:6]
cat("Least Square Means","\n")
mediasf
}

#Función usuario para construcción de los I.C para medias del factor de efectos fijos
#En modelo mixto sin interacción
#El argumento modelo hace referencia al modelo ajustado con la función lmer
mediasfactorialmixtonointerv02=function(modelo,alpha=0.05){
medias=lsmeansLT(modelo)
df=round(medias$df)
tcrit=qt(alpha,df=df,lower.tail=F)
lower=medias$Estimate-tcrit*medias$"Std. Error"
upper=medias$Estimate+tcrit*medias$"Std. Error"
mediasf=data.frame(medias$Estimate,medias$"Std. Error",medias$df,medias$"t value",lower,upper,row.names=dimnames(medias)[[1]])
names(mediasf)=dimnames(medias)[[2]][1:6]
cat("Least Square Means","\n")
mediasf
}

#En la función, el argumento modelo debe ser el objeto obtenido con lmer en modelo mixto con interacción
mediasfactorialmixtointerv02(modelo=modelob)

#En la función, el argumento modelo debe ser el objeto obtenido con lmer en modelo mixto sin interacción
mediasfactorialmixtonointerv02(modelo=modeloc)
#############################################################

#Función para comparaciones por LSD y Tukey en modelo mixto con interacción
#Se asume que el factor A es el de efectos fijos
LSDTukeymixtosinter=function(respuesta,factorfijo,nivelesA,nivelesB,MSAB){
library(agricolae)
DFerror=(nivelesA-1)*(nivelesB-1)
lsd=LSD.test(respuesta,factorfijo,DFerror=DFerror,MSerror=MSAB,console=F,group=F)
hsd=HSD.test(respuesta,factorfijo,DFerror=DFerror,MSerror=MSAB,console=F,group=F)
cat("LSD Test for fixed factor in mixed model with interaction","\n")
print(lsd$parameters)
cat("Comparison between treatments means","\n")
print(lsd$comparison)
cat("\n","\n")
cat("HSD Test for fixed factor in mixed model with interaction","\n")
print(hsd$parameters);cat("Comparison between treatments means","\n")
print(hsd$comparison)
}

LSDTukeymixtosinter(respuesta=cosecha,factorfijo=VariedadTrigo,nivelesA=4,nivelesB=3,MSAB=175.3542)

#Función para comparaciones por LSD y Tukey en modelo mixto sin interacción
LSDTukeymixtosnointer=function(respuesta,factorfijo,nivelesA,nivelesB,MSE,nreplicas){
library(agricolae)
DFerror=nivelesA*nivelesB*nreplicas-nivelesA-nivelesB+1
lsd=LSD.test(respuesta,factorfijo,DFerror=DFerror,MSerror=MSE,console=F,group=F)
hsd=HSD.test(respuesta,factorfijo,DFerror=DFerror,MSerror=MSE,console=F,group=F)
cat("LSD Test for fixed factor in mixed model without interaction","\n")
print(lsd$parameters)
cat("Comparison between treatments means","\n")
print(lsd$comparison)
cat("\n","\n")
cat("HSD Test for fixed factor in mixed model without interaction","\n")
print(hsd$parameters)
cat("Comparison between treatments means","\n")
print(hsd$comparison)
}

#modelo sin interacción, lea valor del MSE y úselo en la función usuario LSDTukeymixtosnointer()
anova(aov(cosecha~VariedadTrigo+Fertilizante))
LSDTukeymixtosnointer(respuesta=cosecha,factorfijo=VariedadTrigo,nivelesA=4,nivelesB=3,MSE=67.35,nreplicas=4)

detach(datos)