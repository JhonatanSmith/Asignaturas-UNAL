###=======================Punto 5====================================

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

#datos5 <- datos5[order(Tipo),]

sapply(split(Tipo,Vida),mean)

datos5
attach(datos5)
costo=ifelse(Tipo=="1",0.985,ifelse(Tipo=="2",0.935,ifelse(Tipo=="3",0.520,0.495)))
Vida.costo=Vida/costo
raizVida=sqrt(Vida)

mediasbat1=sapply(split(Vida.costo,Tipo),mean)
boxplot(Vida.costo~Tipo)
lines(mediasbat1,col=2,type="b",pch=19)

mediasbat2=sapply(split(Vida,Tipo),mean)
boxplot(Vida~Tipo)
lines(mediasbat2,col=2,type="b",pch=19)

mediasbat3=sapply(split(raizVida,Tipo),mean)
boxplot(raizVida~Tipo)
lines(mediasbat3,col=2,type="b",pch=19)



#-- Modelo Anova

modeloanova=aov(Vida~Tipo)
summary(modeloanova)
cbind(Tipo, round(Vida,3))

#qqplot

qqnorm(residuals(modeloanova), main = "")
qqline(residuals(modeloanova))


# TEST DE INDEPENDENCIA
Box.test(Vida, lag = 1, type = c("Ljung-Box"), fitdf = 0)

# TEST DE NORMALIDAD CON RESIDUOS INTERNAMENTE ESTUDENTIZADOS
shapiro.test(rstandard(modeloanova)) 


# TESTS PARA HOMOGENEIDAD DE VARIANZA
bartlett.test(Vida.costo~Tipo)
leveneTest(Vida.costo~Tipo)
cochran.test(Vida.costo~Tipo)

library(dplyr)
group_by(datos5, Tipo) %>% mean()

nrow(datos5)
# Latex
library(xtable)
print(xtable(datos22), include.rownames = FALSE)

# TAMANO DE MUESTRA

#funcon
diftukey2=function(n,a,sigma2,difsig,alpha=0.05){
  gl=a*(n-1)
  dif=sigma2*qtukey(alpha,nmeans=a,df=gl,lower.tail=FALSE)^2-(n*difsig^2)
  dif
} 
#Buscando secuencialmente a n, desde n=2
n=2
#sigma2=MSE
res=diftukey2(n,a = 4,sigma2 = 2374.497,difsig=125,alpha = 0.05)

while(res>=0){
  n=n+1
  res=diftukey2(n,4,2374.497,difsig=125,alpha = 0.05)
}
n.optim=n; n.optim


tabla=data.frame(n=2:n.optim,difer=diftukey2(2:n.optim,4,2374.497,difsig=125), 
                 Acción=ifelse(diftukey2(2:n.optim,4,2374.497,difsig=125)>=0,
                               "Incrementar n","No incrementar n"))
tabla

plot(x = tabla$n, y = tabla$difer)


library(xtable)
print(xtable(tabla), include.rownames = FALSE)


detach(datos5)


########### 13

rm(list=ls(all=TRUE))
datos13=data.frame(Lote=factor(rep(1:5,times=5)),ContCalcio=scan())
23.46 23.59 23.51 23.28 23.29
23.48 23.46 23.64 23.40 23.46
23.56 23.42 23.46 23.37 23.37
23.39 23.49 23.52 23.46 23.32
23.40 23.50 23.49 23.39 23.38

datos13

library(xtable)
print(xtable(datos13), include.rownames = FALSE)

attach(datos13)
mediasLot=sapply(split(ContCalcio,Lote),mean);mediasLot
boxplot(ContCalcio~Lote)
lines(mediasLot,col=2,type="b",pch=19)


#MODELANDO EFECTOS ALEATORIOS PARA OBTENER COMPONENTES DE VARIANZA 
library(lme4)
diseno=lmer(ContCalcio~1|Lote)

detach(datos13)


####----------- Punto 22 ------------

rm(list=ls(all=TRUE))
datos22=data.frame(Marcas=factor(rep(1:5,each=4)),
                   Nivelhumedad=factor(rep(1:4,times=5)),KWh=scan())
685 792 838 875
722 806 893 953
733 802 880 941
811 888 952 1005
828 920 978 1023

datos22

attach(datos22)

mediasMarcas=sapply(split(KWh,Marcas),mean)
mediasMarcas
mediasNivelhumedad=sapply(split(KWh,Nivelhumedad),mean)
mediasNivelhumedad

boxplot(KWh~Marcas,boxwex=0.5,xlab="Marcas",col="bisque")
lines(mediasMarcas,col=2,lty=1,type="b",pch=19)

boxplot(KWh~Nivelhumedad,boxwex=0.5,xlab="Nivelhumedad",col="bisque")
lines(mediasNivelhumedad,col=2,lty=1,type="b",pch=19)

#-- Modelo Anova
modeloanova=aov(KWh~Marcas+Nivelhumedad)
summary(modeloanova)

library(xtable)
print(xtable(summary(modeloanova)), include.rownames = FALSE)


detach(datos22)




