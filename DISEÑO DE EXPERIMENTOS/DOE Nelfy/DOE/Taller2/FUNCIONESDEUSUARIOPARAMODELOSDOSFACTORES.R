#FUNCIÓN DE USUARIO PARA TEST ANOVA PARA LA IGUALDAD DE LAS MEDIAS DE UN FACTOR EN CADA NIVEL DEL OTRO FACTOR
#MODELO CON INTERACCIÓN
#El argumento "modelo" se refiere al modelo factorial de efectos fijos con interacción
#library(lsmeans) debe ser cargada por fuera de la función de usuario y no puede cargarse junto con librería lmerTest
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

#FUNCIÓN DE USUARIO PARA ANOVA DOS FACTORES DE EFECTOS ALEATORIOS CON INTERACCIÓN
#El argumento modelo es el modelo ANOVA corrido como de efectos fijos
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

#FUNCIÓN DE USUARIO PARA ANOVA DOS FACTORES DE EFECTOS MIXTOS CON INTERACCIÓN
#El argumento modelo es el modelo ANOVA corrido como de efectos fijos y el primer factor debe 
#ser el factor que realmente es de efectos fijo
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

#Función usuario para construcción de los I.C para medias del factor de efectos fijos
#En modelo mixto con interacción
#En la función, el argumento modelo debe ser el objeto obtenido con lmer en modelo mixto con interacción
#library(lmerTest) debe ser cargada  por fuera de la función de usuario
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
#En la función, el argumento modelo debe ser el objeto obtenido con lmer en modelo mixto sin interacciónmediasfactorialmixtonointerv02=function(modelo,alpha=0.05){
#library(lmerTest)   debe ser cargada  por fuera de la función de usuario
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

#Función para comparaciones por LSD y Tukey en modelo mixto con interacción
#Se asume que el factor A es el de efectos fijos
#El argumento MSAB es la suma de cuadrados medios de la interacción del modelo
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

#Función para comparaciones por LSD y Tukey en modelo mixto sin interacción
#El argumento MSE es para especificar el MSE del modelo mixto sin interacción
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