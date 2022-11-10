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

# MODELO ANOVA
modeloanova=aov(Vida.costo~Tipo)
summary(modeloanova)

# TEST DE INDEPENDENCIA
Box.test(Vida.costo, lag = 1, type = c("Ljung-Box"), fitdf = 0)

# TEST DE NORMALIDAD CON RESIDUOS INTERNAMENTE ESTUDENTIZADOS
shapiro.test(rstandard(modeloanova)) 


# TESTS PARA HOMOGENEIDAD DE VARIANZA
bartlett.test(Vida.costo~Tipo)
leveneTest(Vida.costo~Tipo)
cochran.test(Vida.costo~Tipo)

#TAMANO DE MUESTRA
#funcon
diftukey2=function(n,a,sigma2,difsig,alpha=0.05){
  gl=a*(n-1)
  dif=qtukey(alpha,nmeans=a,df=gl,lower.tail=FALSE)^2-(n*difsig^2)/sigma2
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


tabla=data.frame(n=2:6,difer=diftukey2(2:6,4,2374.497,difsig=125), 
                 Acción=ifelse(diftukey2(2:6,4,2374.497,difsig=125)>=0,
                               "Incrementar n","No incrementar n"))
tabla

#Comparacion medias

a<-TukeyHSD(modeloanova, conf.level = 0.95) #I.C de Tukey
HSD.test(modeloanova,"Tipo",console = T, group = T,unbalanced = F) #Agrupamiento
library(xtable)
print(xtable(a$Tipo), include.rownames = FALSE)

plot(a,cex.lab=0.8,las=1)

library(agricolae)

scheffe.test(y=modeloanova, trt= "Tipo", 
             DFerror= 12, MSerror=2374.497, Fc= 59.985, 
             alpha = 0.05, group=T, console = T)

#873287.52
scheffe.test(y=modeloanova, trt= "Tipo", 
             DFerror= 12, MSerror=873287.52, Fc= 59.985, 
             alpha = 0.05, group=F, console = T)


print(xtable(b$Tipo), include.rownames = FALSE)
plot(b,cex.lab=0.8,las=1)

library(DescTools)
b<-ScheffeTest(modeloanova)
plot(b)

#HSD
a<-4
n<-4
sigma2<-2374.497
1/sqrt(2)* qtukey(0.05,nmeans=a,df=a*(n-1),lower.tail=FALSE) *sqrt(sigma2/2)
qf(0.05, a - 1, 12,lower.tail = F)

vF = 59.99
#Valor crítico de Scheffe = F Valor crítico * 2
qf(0.05, a - 1, 12,lower.tail = F)*2

(860.69525-571.066)^2/(2374.497*1/2)

