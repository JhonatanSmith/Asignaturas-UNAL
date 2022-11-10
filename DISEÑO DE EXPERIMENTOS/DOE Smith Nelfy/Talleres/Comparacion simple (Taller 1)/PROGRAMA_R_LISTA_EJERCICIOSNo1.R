#Problema 3
rm(list=ls(all=TRUE))
datos3=data.frame(sexo=factor(rep(c("Mujer","Hombre"),each=10)),TC=scan())
75 77 78 79 77 73 78 79 78 80
74 72 77 76 76 73 75 73 74 75

attach(datos3)

boxplot(TC~sexo,boxwex=0.5) #boxplots de las observaciones según factor sexo 
var.test(TC~sexo,alternative="two.sided") #test de homogeneidad de varianza

#test t de igualdad de medias, muestras normales independientes, varianzas poblacionales iguales
t.test(TC~sexo,var.equal =TRUE,alternative="two.sided",paired=FALSE) 

detach(datos3)

#Problema 4
rm(list=ls(all=TRUE))
datos4=data.frame(Temperatura=factor(rep(c("baja","alta"),times=10)),encogimiento=scan())
17.2 21.4
17.5 20.9
18.6 19.8
15.9 20.4
16.4 20.6
17.3 21.0
16.8 20.8
18.4 19.9
16.7 21.1
17.6 20.3

attach(datos4)

boxplot(encogimiento~Temperatura,boxwex=0.5)

var.test(encogimiento~Temperatura,alternative="two.sided")
t.test(encogimiento~Temperatura,var.equal =TRUE,alternative="two.sided",paired=FALSE)

detach(datos4)

#Problema 5
rm(list=ls(all=TRUE))
datos5=data.frame(Ruta=factor(rep(c("A","B"),each=5)),TV=scan())
18 24 30 21 32
22 29 34 25 35

attach(datos5)

boxplot(TV~Ruta,boxwex=0.5)
medias5=sapply(split(TV,Ruta),mean);medias5 #calcunado media muestral según factor ruta
var5=sapply(split(TV,Ruta),var);var5 #calculando varianza muestral según ruta

var.test(TV~Ruta,alternative="two.sided")
t.test(TV~Ruta,var.equal =TRUE,alternative="two.sided",paired=FALSE)

detach(datos5)

#Problema 6
rm(list=ls(all=TRUE))
datos6=data.frame(Proveedor=factor(rep(1:2,each=14)),Diámetro=scan())
21.38 20.13 19.12 19.85 20.54 8.00 22.24
21.94 19.07 18.60 21.89 22.60 18.10 19.25
21.51 22.22 21.49 21.91 21.52 22.06 21.51
21.29 22.71 22.65 21.53 22.22 21.92 20.82

attach(datos6)

boxplot(Diámetro~Proveedor,boxwex=0.5)
abline(h=c(20.25-2.25,20.25,20.25+2.25),lty=2) #líneas horizontales pasando por las especificaciones de diámetros
var6=sapply(split(Diámetro,Proveedor),var);var6

var.test(Diámetro~Proveedor,alternative="two.sided") #¿varianzas iguales?
t.test(Diámetro~Proveedor,var.equal=FALSE,alternative="two.sided",paired=FALSE)

detach(datos6)

#Problema 7
rm(list=ls(all=TRUE))
datos7=data.frame(Bola=factor(rep(c("X","Y"),each=10)),Diámetro=scan())
75 46 57 43 58 32 61 56 34 65
52 41 43 47 32 49 52 44 57 60

attach(datos7)

boxplot(Diámetro~Bola,boxwex=0.5) #¿será válido este análisis?
difer=datos7[,2][Bola=="X"]-datos7[,2][Bola=="Y"] #diferencias por pares
boxplot(difer,boxwex=0.5,xlab="Diferencias por pares") #distribución de la diferencia de pares de observaciones

#¿será válida la siguiente comparación de varianzas y medias según muestras normales independientes?
var.test(Diámetro~Bola,alternative="two.sided")
t.test(Diámetro~Bola,var.equal =TRUE,alternative="two.sided",paired=FALSE)

#Comparación de medias con muestras pareadas
t.test(Diámetro~Bola,var.equal =TRUE,alternative="two.sided",paired=TRUE)

detach(datos7)

#Problema 8
rm(list=ls(all=TRUE))
datos8=data.frame(Aditivo=factor(rep(c("Sin Aditivo","Con Aditivo"),each=10)),Crecimiento=scan())
20 31 16 22 19 32 25 18 20 19
23 34 15 21 22 31 29 20 24 23

attach(datos8)

boxplot(Crecimiento~Aditivo,boxwex=0.5) #¿será válido este análisis?
difer=datos8[,2][Aditivo=="Con Aditivo"]-datos8[,2][Aditivo=="Sin Aditivo"] #diferencia por pares de observaciones
boxplot(difer,boxwex=0.5,xlab="Diferencias por pares") #distribución de las diferencias pareadas

#Test t para muestras pareadas
t.test(Crecimiento~Aditivo,var.equal =TRUE,alternative="two.sided",paired=TRUE)

detach(datos8)

#Problema 9
rm(list=ls(all=TRUE))
datos9=data.frame(Técnico=factor(rep(c(1,2),each=8)),Pureza=scan())
74.0 73.1 73.5 73.9 71.2 72.5 73.0 74.3
73.0 71.3 73.2 71.1 70.3 71.5 73.4 72.4

attach(datos9)

boxplot(Pureza~Técnico,boxwex=0.5) #será válido este análisis?
difer=datos9[,2][Técnico==1]-datos9[,2][Técnico==2] #diferencias entre pares de observaciones
boxplot(difer,boxwex=0.5,xlab="Diferencias por pares") #Distribución de las diferencias pareadas
#Test de igualdad de medias de muestras pareadas ¿será apropiado?
t.test(Pureza~Técnico,var.equal =TRUE,alternative="two.sided",paired=TRUE)

#Test t de con H1:mu_d>0 con muestras pareadas
t.test(Pureza~Técnico,var.equal =TRUE,alternative="greater",paired=TRUE)

detach(datos9)

#Problema 10
rm(list=ls(all=TRUE))
datos10=data.frame(Termómetro=factor(rep(c("Mer","Rtd"),times=25)),Temperatura=scan())
4.0 2.6 4.0 2.8 5.0 5.0 0.5 0.0 3.0 2.4
5.0 6.4 6.0 6.4 2.0 2.3 4.0 4.2 4.0 4.0
4.5 3.3 4.0 1.4 3.5 1.8 2.0 -1.9 3.0 -7.6
2.5 3.1 4.0 5.0 6.5 6.6 4.5 2.7 4.0 6.3
4.0 0.0 4.0 0.4 3.5 0.6 2.0 -4.0 4.0 -6.3

attach(datos10)

plot(datos10) #¿será apropiado este análisis?
difer=datos10[,2][Termómetro=="Mer"]-datos10[,2][Termómetro=="Rtd"]
boxplot(difer,boxwex=0.5,xlab="Diferencias por pares")

#Test t para comparar medias con muestras pareadas
t.test(Temperatura~Termómetro,var.equal=TRUE,alternative="two.sided",paired=TRUE)

detach(datos10)

#Problema 11
rm(list=ls(all=TRUE))
datos11=data.frame(solución=factor(rep(c(1,1,2,2),times=5)),rapidez=scan())
9.9 10.6 10.2 10.0
9.4 10.3 10.6 10.2
9.3 10.0 10.7 10.7
9.6 10.3 10.4 10.4
10.2 10.1 10.5 10.3

attach(datos11)

#para análisis con muestras independientes
boxplot(rapidez~solución,boxwex=0.5) 
var.test(rapidez~solución,alternative="two.sided")

#cuál de los siguientes dos tests es más apropiado?
#test t de igualdad de medias con muestras independientes de poblaciones
#normales con igual varianza
t.test(rapidez~solución,var.equal =TRUE,alternative="two.sided",paired=FALSE)

#test t de igualdad de medias con muestras independientes de poblaciones
#normales con diferente varianza
t.test(rapidez~solución,var.equal =FALSE,alternative="two.sided",paired=FALSE)

detach(datos11)

#Problema 12
rm(list=ls(all=TRUE))
datos12=data.frame(Tipo.Tub=factor(rep(c(1,1,1,2,2,2),times=5)),Temperatura=scan())
206 193 192 177 176 198
188 207 210 197 185 188
205 185 194 206 200 189
187 189 178 201 197 203
194 213 205 180 192 192

attach(datos12)

boxplot(Temperatura~Tipo.Tub,boxwex=0.5)
medias12=sapply(split(Temperatura,Tipo.Tub),mean);medias12
var12=sapply(split(Temperatura,Tipo.Tub),var);var12

var.test(Temperatura~Tipo.Tub,alternative="two.sided")
t.test(Temperatura~Tipo.Tub,var.equal =TRUE,alternative="less",paired=FALSE)

#Cálculo de potencia  pedido
n=15
S2p=(n-1)*sum(var12)/(2*n-2) #estimación pooled de varianza común

ncp=-5*sqrt(15/(2*S2p)) #parámetro de no centraldad
pt(-qt(0.05,df=28,lower.tail=F),df=28,ncp=ncp)

#o bien
pot.t=power.t.test(n=n,delta=5,sd=sqrt(S2p),type = "two.sample",alternative ="one.sided")
pot.t

detach(datos12)

#Problema 13
rm(list=ls(all=TRUE))
datos13=data.frame(Empleado=factor(rep(c("A","B"),times=10)),Tiempo=scan())
27.64 26.75
17.01 29.16
28.97 25.24
19.01 22.97
23.24 30.72
27.07 37.61
27.62 34.62
16.38 31.93
23.99 27.40
25.70 28.36

attach(datos13)

boxplot(Tiempo~Empleado,boxwex=0.5)
var.test(Tiempo~Empleado,alternative="two.sided")

#cuál de los siguientes dos tests es más apropiado?
#test t de igualdad de medias con muestras independientes de poblaciones
#normales con igual varianza
t.test(Tiempo~Empleado,var.equal =TRUE,alternative="two.sided",paired=FALSE)

#test t de igualdad de medias con muestras independientes de poblaciones
#normales con diferente varianza
t.test(Tiempo~Empleado,var.equal =FALSE,alternative="two.sided",paired=FALSE)

detach(datos13)

#Problema 14
rm(list=ls(all=TRUE))
datos14=data.frame(Trat=factor(rep(c("Antes","Despues"),times=15)),Colesterol=scan())
265 229
240 231
258 227
295 240
251 238
245 241
287 234
314 256
260 247
279 239
283 246
240 218
238 219
225 226
247 233

attach(datos14)

boxplot(Colesterol~Trat,boxwex=0.5) #¿será apropiado este análisis?
difer=datos14[,2][Trat=="Antes"]-datos14[,2][Trat=="Despues"] #diferencia por pares de observaciones

boxplot(difer,boxwex=0.5,xlab="Diferencias por pares") #distribución diferencia de las observaciones pareadas

#¿cuál de las siguientes dos pruebas es más apropiada?
#test t de igualdad de medias con muestras pareadas
t.test(Colesterol~Trat,alternative="two.sided",paired=TRUE) 

#test t para con H1:mu_d>0  con muestras pareadas 
#mu_d es la media poblacional de las diferencias en observaciones pareadas "antes-después"
t.test(Colesterol~Trat,alternative="greater",paired=TRUE)

detach(datos14)

#Problema 15
rm(list=ls(all=TRUE))
datos15=data.frame(Trat=factor(rep(c("Antes","Despues"),times=10)),Pérdida.Peso=scan())
195 187
213 195
247 221
201 190
187 175
210 197
215 199
246 221
294 278
310 285

attach(datos15)

boxplot(Pérdida.Peso~Trat,boxwex=0.5) #¿será apropiado este análisis?
difer=datos15[,2][Trat=="Antes"]-datos15[,2][Trat=="Despues"]
boxplot(difer,boxwex=0.5,xlab="Diferencias por pares")

#¿cuál de los siguientes dos tests es el apropiado?
#Test t con muestras pareadas con H0:mu_d<=0 vs. H1:mu_d>0
t.test(Pérdida.Peso~Trat,alternative="greater",paired=TRUE)

#test t con muestras pareadas conH0:mu_d>=10 vs.  H1:mu_d<10
t.test(Pérdida.Peso~Trat,alternative="less",paired=TRUE,mu=10)

#Cálculo de potencia test t con n=10 ¿cuál de los siguientes dos cálculos es apropiado para pregunta  c)? 
power.t.test(n = 10, delta = 10, sd =sd(difer),sig.level = 0.05,type ="paired",alternative ="two.sided")

power.t.test(n = 10, delta = 10, sd =sd(difer),sig.level = 0.05,type ="paired",alternative ="one.sided")

detach(datos15)


