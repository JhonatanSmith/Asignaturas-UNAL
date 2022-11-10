rm(list=ls(all=TRUE))
datos5=data.frame(scan(what=list(Tipo="",Vida=0)))
1 602
2 863
1 529
4 235
1 534
1 585
2 743
3 232
4 282
2 773
2 840
3 255
4 238
3 200
4 228
3 215 

datos5
attach(datos5)
costo=ifelse(Tipo=="1",0.985,ifelse(Tipo=="2",0.935,ifelse(Tipo=="3",0.520,0.495)))
Vida.costo=Vida/costo
raizVida=sqrt(Vida) 

#TESTES PARA HOMOGENEIDAD DE VARIANZA
bartlett.test(Vida.costo~Tipo)
leveneTest(Vida.costo~Tipo)

cochran.test(Vida.costo~Tipo,data = datos5$)

library(gmodels)
library(multcomp)
library(daewr)
library(car)
library(outliers)

modeloanova=aov(Vida.costo~Tipo)
summary(modeloanova)



modelo2 = aov(Vida.costo~Tipo)

#OBTENIENDO GR?FICOS DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS,
layout(rbind(c(1,1,2,2),c(0,3,3,0)))
stripchart(rstandard(modelo2)~Tipo,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="Lb N/acre")
abline(h=c(-2,0,2),lty=2)
plot(fitted(modelo2),rstandard(modelo2),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(modelo2))
qqline(rstandard(modelo2),lty=2)

#TEST DE NORMALIDAD CON RESIDUOS INTERNAMENTE ESTUDENTIZADOS
shapiro.test(rstandard(modelo2)) 

# Prueba

mismediastratamientos(modeloanova = modelo2, nivel = 95)

mediasy2=sapply(split(Vida.costo,Tipo),mean)


#INTERVALOS DE TUKEY
TukeyHSD(modelo2, conf.level = 0.95) #Funci?n es cargada por defecto por librer?a stats 
plot(TukeyHSD(modelo2, conf.level = 0.95),cex.lab=0.8)


#DEFINIENDO FUNCIÃON USUARIO PARA TEST LJUNG-BOX

BP.LB.test=function(serie,maxlag,type="Box"){
  aux=floor(maxlag/6); X.squared=c(rep(NA,aux))
  df=c(rep(NA,aux)); p.value=c(rep(NA,aux))
  for(i in 1:aux){
    test=Box.test(serie,lag=(6*i),type=type)
    X.squared[i]=test[[1]]; df[i]=test[[2]]
    p.value[i]=test[[3]]
  }
  lag=6*c(1:aux)
  teste=as.data.frame(cbind(X.squared,df,p.value))
  rownames(teste)=lag; teste
}

residuales = residuals(modelo2)

prueba_1a<-BP.LB.test(residuales,36,type = "Box")

##################################################################

E) ¿Cuando una de las baterias es mas economica que las otras?
  
  ¿Existen diferencias entre la vida media x coste de marca A vs Almacen?
  
  ¿cuáles son más económicas?
  

  COSTOS ASOCIADOS:                      Media bateria   Rendimiento/Unidad
  
  Alcalina A = 0.985 --------------1       571.0660           571.0660*0.985
  
  Alcalina Almacen = 0.935---------2        860.6952          860.6952*0.935
  
  LDuracion A = 0.520--------------3       433.6538            433.6538*0.520
    
  LDuracion Almacen = 0.495--------4        443.3793          443.3793*0.495
  
  
attach(datos5)

lsmeans(modeloanova,~Tipo)

En el modeloanova se tiene un modelo ajustado con los datos del cociente entre 
Vida/costo. Si el costo es muy grande respecto a la vida, entonces este valor es 
pequeño.

Si el costo es pequeño respecto a la vida media de la bateria, entonces el valor
será grande

Dado este analisis, una bateria será mas econommica que la otra, si la media de 
este cociente dado el anterioir modelo es mas grande respecto a la otra. 

 # Para esto, se deben realizar pruebas de hipotesis pertinentes.

summary(modeloanova)


##########################################################################


inciso d.

paco = aov(Vida~Tipo)
summary(paco)

# prueba de normalidad para datos no escalados sqrt vida

shapiro.test(rstandard(paco)) 


# Prueba de igualdad de varianzas raiz vida

bartlett.test(Vida.costo~Tipo)

# grafico de supuestos para modelo datos escalados

#OBTENIENDO GR?FICOS DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS,
layout(rbind(c(1,1,2,2),c(0,3,3,0)))
stripchart(rstandard(paco)~Tipo,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="Tipo de bateria")
abline(h=c(-2,0,2),lty=2)
plot(fitted(paco),rstandard(paco),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(paco))
qqline(rstandard(paco),lty=2)



##EFECTO INDIVIDUAL DE LA RTA MEDIA DE CADA NIVEL PARA PROBLEMA DE BATERIAS

# diseño
diseno=aov(aov(Vida.costo ̃Tipo))
anova(diseno)

#ESTIMACI?N DE LOS EFECTOS PRINCIPALES:
model.tables(diseno,type = "effects",se=TRUE)


# CALCULO EFECTOS PRINCIPALES

model.tables(modelo2,type = "effects",se=TRUE)


#C?LCULO INDIVIDUAL DE EFECTOS, SUS TESTES T Y I.C DEL 95%:

# Sea 

modelo2 = aov(Vida.costo~Tipo)

efect.1=fit.contrast(modelo2,"Tipo",
                     rbind(":"=c(3/4,-1/4,-1/4,-1/4)),conf=0.95)
efect.2=fit.contrast(modelo2,"Tipo",
                     rbind(":efecto 2"=c(-1/4,3/4,-1/4,-1/4)),conf=0.95)
efect.3=fit.contrast(modelo2,"Tipo",
                     rbind(":efecto 3"=c(-1/4,-1/4,3/4,-1/4)),conf=0.95)
efect.4=fit.contrast(modelo2,"Tipo",
                     rbind(":efecto 4"=c(-1/4,-1/4,-1/4,3/4)),conf=0.95)

rbind(efect.1,efect.2,efect.3,efect.4)

# OTRA FORMA DE CALCULAR LOS EFECTOS INDIVIDUALES DEL MODELO

Tipo2=as.factor(Tipo)
Vida.costo

caroteamo =aov(Vida.costo~Tipo2)

#OTRA FORMA PARA C?LCULO INDIVIDUAL DE EFECTOS CON SUS INTERVALOS DE CONFIANZA DEL 95%:
contr1=rbind("efecto Alcalina  A"= c(3/4,-1/4,-1/4,-1/4))
contr2=rbind("efecto Alcalina Almacen"= c(-1/4,3/4,-1/4,-1/4))
contr3=rbind("efecto LDuracion A"= c(-1/4,-1/4,3/4,-1/4))
contr4=rbind("efecto LDuracion Almacen"= c(-1/4,-1/4,-1/4,3/4))

rbind(confint(glht(caroteamo,linfct=mcp(Tipo2=contr1)))$confint,
      confint(glht(caroteamo,linfct=mcp(Tipo2=contr2)))$confint,
      confint(glht(caroteamo,linfct=mcp(Tipo2=contr3)))$confint,
      confint(glht(caroteamo,linfct=mcp(Tipo2=contr4)))$confint)

# MODELO PARA RAIZ DE VIDA

anateamo = aov(raizVida~Tipo)

# Analisis descriptivo con los graficos

#OBTENIENDO GR?FICOS DE RESIDUOS INTERNAMENTE ESTUDENTIZADOS,
layout(rbind(c(1,1,2,2),c(0,3,3,0)))
stripchart(rstandard(anateamo)~Tipo,vertical=TRUE,ylim=c(-2.5,2.5),pch=1,cex=1,xlab="Tipo de bateria")
abline(h=c(-2,0,2),lty=2)
plot(fitted(anateamo),rstandard(anateamo),ylim=c(-2.5,2.5))
abline(h=c(-2,0,2),lty=2)
qqnorm(rstandard(anateamo))
qqline(rstandard(anateamo),lty=2)


# prueba de normalidad para datos no escalados sqrt vida

shapiro.test(rstandard(anateamo)) 


# Prueba de igualdad de varianzas raiz vida

bartlett.test(raizVida~Tipo)


# Ejercicio 13:

  *Lectura de datos*
  
  
detach(datos12)
rm(list=ls(all=TRUE))
datos13=data.frame(Lote=factor(rep(1:5,times=5)),ContCalcio=scan())
23.46 23.59 23.51 23.28 23.29
23.48 23.46 23.64 23.40 23.46
23.56 23.42 23.46 23.37 23.37
23.39 23.49 23.52 23.46 23.32
23.40 23.50 23.49 23.39 23.38
datos13
attach(datos13)

mediasLot=sapply(split(ContCalcio,Lote),mean);
mediasLot

boxplot(ContCalcio~Lote)

lines(1:5,mediasLot,col=2,type="b",pch=19) 

# Ejercicio 13:




