library(gmodels)
library(multcomp)
library(daewr)
library(car)
library(outliers)
library(lsmeans)
library(agricolae)
library(lme4)

rm(list=ls(all=TRUE))
datos2=data.frame(tratamiento=factor(factor(rep(c("T1","T2","T3","T4"),times=7))),Tiempo=scan())
213 76 57 84
214 85 67 82
204 74 55 85
208 78 64 92
212 82 61 87
200 75 63 79
207 82 63 90

attach(datos2)
mediast=sapply(split(Tiempo,tratamiento),mean)
boxplot(Tiempo~tratamiento,boxwex=0.5)
lines(mediast,col=2,type="b",pch=19)

detach(datos2)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos3=data.frame(Tableta=factor(rep(c("A","B","C","D","E"),times=5)),horas=scan())
5.2 9.1 3.2 2.4 7.1
4.7 7.1 5.8 3.4 6.6
8.1 8.2 2.2 4.1 9.3
6.2 6.0 3.1 1.0 4.2
3.0 9.1 7.2 4.0 7.6

datos3
attach(datos3)
mediasTab=sapply(split(horas,Tableta),mean)
boxplot(horas~Tableta,boxwex=0.5)
lines(mediasTab,col=2,type="b",pch=19)

detach(datos3)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos4=data.frame(Mezcla=factor(rep(1:5,times=6)),humedad=scan())
551 595 639 417 563
457 580 615 449 631
450 508 511 517 522
731 583 573 438 613
499 633 648 415 656
632 517 677 555 679

datos4
attach(datos4)
mediasMez=sapply(split(humedad,Mezcla),mean)
boxplot(humedad~Mezcla,boxwex=0.5)
lines(mediasMez,col=2,type="b",pch=19)

detach(datos4)
#-------------------------------------------------------------
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

mediasbat1=sapply(split(Vida.costo,Tipo),mean)
boxplot(Vida.costo~Tipo)
lines(mediasbat1,col=2,type="b",pch=19)

mediasbat2=sapply(split(Vida,Tipo),mean)
boxplot(Vida~Tipo)
lines(mediasbat2,col=2,type="b",pch=19)

mediasbat3=sapply(split(raizVida,Tipo),mean)
boxplot(raizVida~Tipo)
lines(mediasbat3,col=2,type="b",pch=19)

detach(datos5)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos6=data.frame(Laboratorio=factor(rep(c("A","B","C","D"),times=5)),medida=scan())
58.7 62.7 55.9 60.7
61.4 64.5 56.1 60.3
60.9 63.1 57.3 60.9
59.1 59.2 55.2 61.4
58.2 60.3 58.1 62.3

datos6
attach(datos6)
medias=sapply(split(medida,Laboratorio),mean);medias
vars=sapply(split(medida,Laboratorio),var);vars

boxplot(medida~Laboratorio,boxwex=0.3,xlab="Laboratorio")
lines(medias,col=2,type="b",pch=19)

detach(datos6)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos7=data.frame(diseño=factor(rep(1:4,each=5)),ventas=scan())
11 17 16 14 15
12 10 15 19 11
23 20 18 17 18
27 33 22 26 28

datos7
attach(datos7)
medias=sapply(split(ventas,diseño),mean);medias
vars=sapply(split(ventas,diseño),var);vars

boxplot(ventas~diseño,boxwex=0.3,xlab="diseño del empaque")
lines(medias,col=2,type="b",pch=19)

detach(datos7)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos8=data.frame(Marca=factor(rep(1:6,each=8)),ContSodio=scan())
24.4 22.6 23.8 22.0 24.5 22.3 25.0 24.5
10.2 12.1 10.3 10.2 9.9 11.2 12.0 9.5
19.2 19.4 19.8 19.0 19.6 18.3 20.0 19.4
17.4 18.1 16.7 18.3 17.6 17.5 18.0 16.4
13.4 15.0 14.1 13.1 14.9 15.0 13.4 14.8
21.3 20.2 20.7 20.8 20.1 18.8 21.1 20.3

datos8
attach(datos8)
mediasM=sapply(split(ContSodio,Marca),mean);mediasM
boxplot(ContSodio~Marca)
lines(mediasM,col=2,type="b",pch=19)

detach(datos8)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos9=data.frame(CantPolvoHornear=factor(rep(c(0.25,0.5,0.75,1.0),times=4)),AumentoAltura=scan())
11.4 27.8 47.6 61.6
11.0 29.2 47.0 62.4
11.3 26.8 47.3 63.0
9.5 26.0 45.5 63.9

datos9

attach(datos9)

mediasCantPolvoHornear=sapply(split(AumentoAltura,CantPolvoHornear),mean)
mediasCantPolvoHornear

boxplot(AumentoAltura~CantPolvoHornear,boxwex=0.5,xlab="CantPolvoHornear",col="bisque")
lines(mediasCantPolvoHornear,col=2,type="b",pch=19)

detach(datos9)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos10=data.frame(Marca=factor(rep(1:6,each=8)),ContSodio=scan())
24.4 22.6 23.8 22.0 24.5 22.3 25.0 24.5
10.2 12.1 10.3 10.2 9.9 11.2 12.0 9.5
19.2 19.4 19.8 19.0 19.6 18.3 20.0 19.4
17.4 18.1 16.7 18.3 17.6 17.5 18.0 16.4
13.4 15.0 14.1 13.1 14.9 15.0 13.4 14.8
21.3 20.2 20.7 20.8 20.1 18.8 21.1 20.3

datos10
attach(datos10)
mediasM=sapply(split(ContSodio,Marca),mean);mediasM
boxplot(ContSodio~Marca)
lines(mediasM,col=2,type="b",pch=19)

detach(datos10)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos11=data.frame(Punto=factor(rep(c("L1","L2","L3","L4"),each=5)),ContO2=scan())
5.9 6.1 6.3 6.1 6.0
6.3 6.6 6.4 6.4 6.5
4.8 4.3 5.0 4.7 5.1
6.0 6.2 6.1 5.8 5.9

datos11

attach(datos11)
mediasP=sapply(split(ContO2,Punto),mean);mediasP
boxplot(ContO2~Punto)
lines(mediasP,col=2,type="b",pch=19)

detach(datos11)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos12=data.frame(Lab=factor(rep(1:4,times=10)),VCposit=scan())
0.888 1.065 1.325 1.232
0.983 1.226 1.069 1.127
1.047 1.332 1.219 1.051
1.087 0.958 0.958 0.897
1.125 0.816 0.819 1.222
0.997 1.015 1.140 1.125
1.025 1.071 1.222 0.990
0.969 0.905 0.995 0.875
0.898 1.140 0.928 0.930
1.018 1.051 1.322 0.775

datos12
attach(datos12)
mediasL=sapply(split(VCposit,Lab),mean);mediasL
boxplot(VCposit~Lab)
lines(mediasL,col=2,type="b",pch=19)

detach(datos12)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos13=data.frame(Lote=factor(rep(1:5,times=5)),ContCalcio=scan())
23.46 23.59 23.51 23.28 23.29
23.48 23.46 23.64 23.40 23.46
23.56 23.42 23.46 23.37 23.37
23.39 23.49 23.52 23.46 23.32
23.40 23.50 23.49 23.39 23.38

datos13
attach(datos13)
mediasLot=sapply(split(ContCalcio,Lote),mean);mediasLot
boxplot(ContCalcio~Lote)
lines(mediasLot,col=2,type="b",pch=19)

detach(datos13)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos14=data.frame(operador=factor(rep(1:4,times=4)),producción=scan())
175.4 168.5 170.1 175.2
171.7 162.7 173.4 175.7
173.0 165.0 175.7 180.1
170.5 164.1 170.7 183.7

datos14

attach(datos14)
medias=sapply(split(producción,operador),mean);medias
vars=sapply(split(producción,operador),var);vars

boxplot(producción~operador,boxwex=0.3,xlab="operador")
lines(medias,col=2,type="b",pch=19)
detach(datos14)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos15=data.frame(MOLDE=factor(rep(1:3,times=10)),TENSION=scan())
88.0 85.9 94.2
88.0 88.6 91.5
94.8 90.0 92.0
90.0 87.1 96.5
93.0 85.6 95.6
89.0 86.0 93.8
86.0 91.0 92.5
92.9 89.6 93.2
89.0 93.0 96.2
93.0 87.5 92.5


datos15

attach(datos15)
medias=sapply(split(TENSION,MOLDE),mean);medias

boxplot(TENSION~MOLDE,boxwex=0.3,xlab="Molde")
lines(medias,col=2,type="b",pch=19)
detach(datos15)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos16=data.frame(Lote=factor(rep(c("LT1","LT2","LT3","LT4","LT5","LT6","LT7","LT8"),each=4)),Aflatoxin=scan())
39 57 63 66
56 13 25 31
64 83 88 71
29 55 21 51
38 66 53 81
11 49 34 10
23 0 5 20
10 11 23 37


datos16
attach(datos16)
medias=sapply(split(Aflatoxin,Lote),mean);medias

boxplot(Aflatoxin~Lote,boxwex=0.5,ylab="Aflatoxin (ppb)",xlab="Lote")
lines(medias,type="b",lty=2,pch=19)

detach(datos16)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos17=data.frame(ContGrasa=factor(rep(c("ExtBaja","MedBaja","ModBaja"),times=5)),Edades=factor(rep(c("15–24","25-34","35-44","45-54","55-64"),each=3)),ReducLipidos=scan())
0.73 0.67 0.15
0.86 0.75 0.21
0.94 0.81 0.26
1.40 1.32 0.75
1.62 1.41 0.78

datos17

attach(datos17)
mediasContGrasa=sapply(split(ReducLipidos,ContGrasa),mean);mediasContGrasa
mediasEdades=sapply(split(ReducLipidos,Edades),mean);mediasEdades

boxplot(ReducLipidos~ContGrasa,boxwex=0.5,xlab="Contenido Grasa")
lines(mediasContGrasa,type="b",lty=2,pch=19)

boxplot(ReducLipidos~Edades,boxwex=0.5,xlab="Edades")
lines(mediasEdades,type="b",lty=2,pch=19)

detach(datos17)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos18=data.frame(Silos=factor(rep(c("A","B","C","D","E"),each=5)),Día=factor(rep(c("1","2","3","4","5"),times=5)),temperatura=scan())
4.0 4.0 5.0 2.5 3.0
5.0 6.0 2.0 4.0 4.0
4.5 4.0 3.5 2.0 3.0
2.5 4.0 6.5 4.5 4.0
4.0 4.0 3.5 2.0 4.0

datos18

attach(datos18)

mediasSilos=sapply(split(temperatura,Silos),mean);mediasSilos
mediasDía=sapply(split(temperatura,Día),mean);mediasDía

boxplot(temperatura~Silos,boxwex=0.5,xlab="Silos")
lines(mediasSilos,type="b",lty=2,pch=19,col=2)

boxplot(temperatura~Día,boxwex=0.5,xlab="Día")
lines(mediasDía,type="b",lty=2,pch=19,col=2)


detach(datos18)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos19=data.frame(DosisVitaC=factor(rep(c("A","B","C"),times=4)),GrupoEdad=factor(rep(1:4,each=3)),DiasRecup=scan())
3 7 4
4 9 6
2 3 3
6 10 7

datos19

attach(datos19)
mediasDosisVitaC=sapply(split(DiasRecup,DosisVitaC),mean);mediasDosisVitaC
mediasGrupoEdad=sapply(split(DiasRecup,GrupoEdad),mean);mediasGrupoEdad

boxplot(DiasRecup~DosisVitaC,boxwex=0.5,xlab="Dosis de Vitamína C")
lines(mediasDosisVitaC,type="b",lty=2,pch=19,col=2)

boxplot(DiasRecup~GrupoEdad,boxwex=0.5,xlab="Grupo de edad")
lines(mediasGrupoEdad,type="b",lty=2,pch=19,col=2)

detach(datos19)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos20=data.frame(Método=factor(rep(1:3,times=10)),bloque=factor(rep(1:10,each=3)),NivelCompet=scan())
73 81 92
76 78 89
75 76 87
74 77 90
76 71 88
73 75 86
68 72 88
64 74 82
65 73 81
62 69 78

datos20

attach(datos20)

mediasMétodo=sapply(split(NivelCompet,Método),mean);mediasMétodo
mediasbloque=sapply(split(NivelCompet,bloque),mean);mediasbloque
boxplot(NivelCompet~Método,boxwex=0.5,xlab="Método")
lines(mediasMétodo,type="b",lty=2,pch=19,col=2)

boxplot(NivelCompet~bloque,boxwex=0.5,xlab="Bloque")
lines(mediasbloque,type="b",lty=2,pch=19,col=2)

detach(datos20)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos21=data.frame(Velocidad=factor(rep(c(5,10,15,20),each=4)),Horno=factor(rep(1:4,times=4)),tamañograno=scan())
16 7 6 10
20 17 13 10
21 11 12 17
24 11 16 17

datos21

attach(datos21)

mediasVelocidad=sapply(split(tamañograno,Velocidad),mean)
mediasVelocidad
mediasHorno=sapply(split(tamañograno,Horno),mean)
mediasHorno

boxplot(tamañograno~Velocidad,boxwex=0.5,xlab="Velocidad",col="bisque")
lines(mediasVelocidad,type="b",lty=2,pch=19,col=2)

boxplot(tamañograno~Horno,boxwex=0.5,xlab="Horno",col="bisque")
lines(mediasHorno,type="b",lty=2,pch=19,col=2)

detach(datos21)
#-------------------------------------------------------------
rm(list=ls(all=TRUE))
datos22=data.frame(Marcas=factor(rep(1:5,each=4)),Nivelhumedad=factor(rep(1:4,times=5)),KWh=scan())
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

detach(datos22)




