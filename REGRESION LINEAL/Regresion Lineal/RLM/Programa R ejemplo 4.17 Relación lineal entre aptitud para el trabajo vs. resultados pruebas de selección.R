library(car)
library(rsm)
library(rgl)
library(perturb)
library(leaps)
library(scatterplot3d)
library(GGally)
ggplot2::theme_set(ggplot2::theme_bw()) 

#CREANDO FUNCIÓN PARA EXTRAER COEFICIENTES ESTIMADOS SUS IC DEL 95%, VIF'S Y COEFICIENTES ESTANDARIZADOS
miscoeficientes=function(modeloreg,datosreg){
coefi=coef(modeloreg)
datos2=as.data.frame(scale(datosreg))
coef.std=c(0,coef(lm(update(formula(modeloreg),~.+0),datos2)))
limites=confint(modeloreg,level=0.95)
vifs=c(0,vif(modeloreg))
resul=data.frame(Estimación=coefi,Limites=limites,Vif=vifs,Coef.Std=coef.std)
cat("Coeficientes estimados, sus I.C, Vifs y Coeficientes estimados estandarizados","\n")
resul
}


###INGRESO DE DATOS POR TECLADO
###COL1="punt.Aptitud",COL2="Prueba1",COL3="Prueba2",COL4="Prueba3",COL5="Prueba4"
datos=data.frame(scan(what=list(punt.Aptitud=0,Prueba1=0,Prueba2=0,Prueba3=0,Prueba4=0)))
94 122 121 96 89
71 108 115 98 78
82 120 115 95 90
76 118 117 93 95
111 113 112 109 109
64 112 96 90 88
109 109 129 102 108
104 112 119 106 105
80 115 101 95 88
73 111 95 95 84
127 119 118 107 110
88 112 110 100 87
99 120 89 105 97
80 117 118 99 100
99 109 125 108 95
116 116 122 116 102
100 104 83 100 102
96 110 101 103 103
126 117 120 113 108
58 120 77 80 74

attach(datos)

#MATRIZ DE DISPERSIÓN
#Una matriz de dispersión más completa

#Matriz de dispersión con histogramas en la diagonal
gg2<-ggpairs(datos,upper=list(continuous = wrap("smooth",alpha = 0.3, size=1.2,method = "lm")),lower=list(continuous ="cor"))
for(i in 1:ncol(datos)){
gg2[i,i]<-gg2[i,i]+
geom_histogram(breaks=hist(datos[,i],breaks = "FD",plot=F)$breaks,
               colour = "red",fill="lightgoldenrod1")
}
win.graph()
gg2

win.graph()
ggpairs(datos,diag=list(continuous=wrap("box_no_facet",color="red",fill="lightgoldenrod1",alpha=0.3)),upper=list(continuous = wrap("smooth",alpha = 0.3, size=1.2,method = "lm")),lower=list(continuous ="cor"))




modelo=lm(punt.Aptitud~Prueba1+Prueba2+Prueba3+Prueba4)
#O bien
modelo=lm(punt.Aptitud~.,datos)
summary(modelo)

miscoeficientes(modelo,datos)


#Anova del modelo
anova(rsm(punt.Aptitud~FO(Prueba1,Prueba2,Prueba3,Prueba4)))

Anova(modelo) #Produce suma de cuadrados SS2
anova(modelo) #Produce sumas de cuadrados SS1

#Lo siguiente produce de forma individual lo reportado en Tabla 4.9
#pero no necesita correrlo
linearHypothesis(modelo,"Prueba1=0") #Prueba F, Beta1=0
linearHypothesis(modelo,"Prueba2=0") #Prueba F, beta2=0
linearHypothesis(modelo,"Prueba3=0") #Prueba F, beta3=0
linearHypothesis(modelo,"Prueba4=0") #Prueba F, beta4=0

linearHypothesis(modelo,c("Prueba1=0","Prueba2=0")) #Prueba F simult?nea, Beta1=0 y Beta2=0, 

##GRÁFICOS DE RESIDUALES
residualPlots(modelo,tests=FALSE,type="rstudent",quadratic=FALSE,col=2,cex=1.5)


test=shapiro.test(rstudent(modelo)) #Test de normalidad sobre residuales estudentizados
qqnorm(rstudent(modelo),cex=2)
qqline(rstudent(modelo),col=2)
legend("topleft",legend=rbind(c("Statistic W","p.value"),round(c(test$statistic,test$p.value),digits=5)),cex=1.2)

####MEDIDAS DE INFLUENCIA 
influence.measures(modelo)
influence.measures(modelo)$is.inf

infIndexPlot(modelo)
influencePlot(modelo,xlim=c(0,1),ylim=c(-2.5,2.5))

dfbetaPlots(modelo,intercept=T,cex=2,col=2)

