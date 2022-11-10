# Ejemplo 1 diapositivas

Se prueban 10 piezas en 2 niveles de temperatura (alto y bajo). Los resultados
son los dados en la tabla.

# Solucion:

* ¿ Cuantos tratamientos estan involucrados?
  
  Existen dos tratamientos y corresponden a dos temperaturas.  (Alta y baja)

  El factor es temperatura.
  
  En ningun momento se habla de seleccion al azar de los niveles (las dos 
  temperaturas) por tanto se asume que se asume que son los dos unicos niveles 
  de interes.
  
  Hay dos tratamientos de efectos fijos.
  
  Hay un factor (temperatura) de efecto fijo con dos niveles 
  
* ¿ Estructura de diseño?
  
  ¿Las unidades experimientales, son particionadas? No. Se asume que las unds 
  exxperimentales son seleccionadas completamente al azar, no son particionadas,
  se asignan tambien al azar a los tratamientos y tiene que haber homogeneidad en
  los resultados.

Al analizar unicamente los Boxplot se ve un claro efecto de la temperatura en el 
encogimiento del dispositivo. Para la prueba de hipotesis, uno de los dos alpfa-i
debe de ser distinto de cero pues si hay una diferencia entre las medias. 

Se espera medias diferentes.


* DATOS:


rm(list=ls(all=TRUE))
# Lectura de datos 

datos4=data.frame(Temperatura=factor(rep(c("baja","alta"),times=10)),encogimiento=scan())

# la funcion scan lee los datos en filas, en este caso leeria el primero que es un datos
# correspondiente a temperatura baja, el siguiente es de alta y asi. Para ello, el 
#codigo del replicate genera un vector de comandos baja-alta intercalados y eso lo tras
#lapa con los datos obtenidos. 

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
#sapply es un apply para una lista de datos
# split devide los datos de encogimiento por categoria. IMPORTANTE

medias=sapply(split(encogimiento,Temperatura),mean)

medias
# lA TEMPERATURA PARA ALTA ES 20.62 Y BAJA ES 17.24

mean(encogimiento) #promedio global es 18.93

vari=sapply(split(encogimiento,Temperatura),var); vari
# Varianza es de 0.2706667 para alta y 0.709333 para baja

boxplot(encogimiento~Temperatura,boxwex=0.5) #Bocplot calculado

En apariencia, no se cumple el supuesto de varianza constante. Pues hay una diferencia
relativamente grande entre las varianzas de cada factor. 

 # Si se plantea la hipotesis H0: sigma_1^2 = sigma_2^2 vs H1: Diferentes...

 Esto se hace con el estadistico F

El sgt codigo probará si hay o no hay varianzas iguales

La funcion var.test() es la prueba F para comparar dos varianzas.

var.test(encogimiento~Temperatura,alternative="two.sided") 

"Este codigo hace prueba de hipotesis de igualdad de varianza y da un IC al 95"

F = 0.38158, num df = 9, denom df = 9, p-value = 0.1674

Notar que el valor P es grande. De hecho, con una significancia del 5% se tiene que

 p-value > alfa entonces NO se rechaza H0  y se conclye que son iguales las varianzas 

95 percent confidence interval:
  0.09477881 1.53623461

 Finalmente, a pesar de que numericamente hay una diferencia entre esas dos varianzas
 no es una diferencia significativa entre ellas. 

  
 # Si se plantea H0: Mu1 = Mu2 = 0 vs H1: Diferentes... 
 
 Esto se hace con el estadistico T0 para esto...
 
 La funcion t.test  permite realizar comparaciones de medias en R
 
t.test(encogimiento~Temperatura,var.equal=TRUE,alternative="two.sided",
        paired=FALSE) 

RESULTADOS:
  
  t = 10.797, df = 18, p-value = 2.71e-09
  
alternative hypothesis: true difference in means is not equal to 0  

95 percent confidence interval:
  2.722307 4.037693
    
mean in group alta mean in group baja 
20.62              17.24 

De estos resultados se tiene que:
  
 * Como el P-value es casi cero, se rechaza la hipotesis nula a favor de la alterna
 
 * El IC calculado es el de Miu1 - Miu2 por tanto, si el cero no está contenido en
Dicho IC entonces se rechaza H0 por tanto se concluye que las medias son distintas


 SOLUCION MEDIANTE LOS MODELOS ANOVA (ASUME VARIANZA IGUAL)
 
# Prueba de hipotesis planteada:
 
 H0: Miu1=Miu2 vs H1: Diferentes...
 
 H0: alfa1=alfa2= 0 vs H1: almenos uno diferente de cero
 
modeloanova=aov(encogimiento~Temperatura) # Use esa fn y le saca summary
summary(modeloanova) 
 
 RESULTADOS:
   
             Df Sum Sq Mean Sq F value   Pr(>F)    
 Temperatura  1  57.12   57.12   116.6 2.71e-09 ***
 Residuals   18   8.82    0.49   
 
De la anterioir tabla se tiene que:
  
  * Mean sq = Sum sq / Df = 57.12 / 1 = 57.12

NOTA: SSA = Sum sq

  * Mean sq = Sum sq / Df = 8.82/18 = 0.49

NOTA: Esto ultimo será el MSE del modelo 

  * Finalmente, allí está el valor p para decidir en la prueba de hipotesis.

Se concluye que se rechaza H0 a favor de H1 en ambos casos, entonces ambas medias 
tienen que ser diferentes.

# CALCULO DE MEDIAS PUNTUALES YA QUE SON DIFERENTES

library(lsmeans)

lsmeans(modeloanova,~Temperatura) 

RESULTADOS: Da las medias y los intervalos de confianza para cada media
  
Temperatura lsmean    SE df lower.CL upper.CL
alta          20.6 0.221 18     20.2     21.1
baja          17.2 0.221 18     16.8     17.7

Confidence level used: 0.95

#DIFERENCIA DE MEDIAS DE TRATAMIENTOS
library(gmodels)
fit.contrast(modeloanova,"Temperatura",rbind(":Alto menos Bajo"=c(1,-1)),conf=0.95)


# SOLUCION MEDIANTE MODELO DE REGRESION CON MEDIAS DE TRATAMIENTO

Primero, se debe ajustar el modelo de regresion...

mrlm1=lm(encogimiento~-1+Temperatura)
summary(mrlm1) 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
Temperaturaalta  20.6200     0.2214   93.15   <2e-16 ***
Temperaturabaja  17.2400     0.2214   77.88   <2e-16 ***
  
Estas son las medias estimadas con sus respectivos errores estandares 

confint(mrlm1)

                  2.5 %   97.5 %
Temperaturaalta 20.15494 21.08506 #IC para miu 1
Temperaturabaja 16.77494 17.70506 #IC para miu 2

# Prueba de igualdad de medias  H0 iguales H1 diferentes

library(car)
linearHypothesis(mrlm1,c("Temperaturaalta-Temperaturabaja=0")) 

# R denomina a los factores como ese vector. Veru summary modelo

Model 2: encogimiento ~ -1 + Temperatura # Se quita intercepto y se usa indicadoras

Res.Df    RSS Df Sum of Sq      F   Pr(>F)    
1     19 65.942                                 
2     18  8.820  1    57.122 116.58 2.71e-09 *** # Se rechaza H0 a favor de H1
 
  fila 1: 19 son grados de libertad del SSE(MR), 65.942 G.L(MR),  
  Fila 2: 18 G.L (SSE(MF)), 8.820 es el SSE(MF)
  Sum of sq es SSE(MR)-SSE(MF)
  ese 1 es el GL de GL(SSE(MR)-SSE(MF))
  
  
# Solucion mediante modelo de regresion con efectos de tratamientos: 
#Asume la igualdad de varianzas

I1=ifelse(Temperatura=="alta",1,0); I2=ifelse(Temperatura=="baja",1,0)
X=I1-I2 #desde que n_1=n_2
mrlm2=lm(encogimiento~X); 
summary(mrlm2) 
  
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  18.9300     0.1565   120.9  < 2e-16 ***
X             1.6900     0.1565    10.8 2.71e-09 ***
  
El 18.93 es el promedio de todos los datos
El 1.6900 es el alfa y en esa fila, ese es el valor P para esa prueba de hipotesis
  
# Analisis de normalidad en los residuos 

#GRÁFICOS DE RESIDUOS ESTUDENTIZADOS INTERNAMENTE
layout(rbind(c(1,1,2,2),c(3,3,4,4)))
plot(fitted(modeloanova),rstandard(modeloanova),main="residuos estudentizados vs. ajustados\nModelo ANOVA",
     pch=as.numeric(Temperatura),col=as.numeric(Temperatura),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("top",legend=c(expression(paste(T^o,sep=" ","alta")),
                      expression(paste(T^o,sep=" ","baja"))),pch=1:2,col=1:2,bg="cornsilk")
stripchart(rstandard(modeloanova)~Temperatura,main="residuos estudentizados vs. Temperatura\nModelo ANOVA",
           xlab="Temperatura",vertical=T, pch=1:2,col=1:2,cex=1.5)
abline(h=c(-2,0,2),lty=2)
plot(fitted(mrlm2),rstandard(mrlm2),main="residuos estudentizados vs. ajustados\nModelo de efectos con MRL",
     pch=as.numeric(Temperatura),col=as.numeric(Temperatura),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("top",legend=c(expression(paste(T^o,sep=" ","alta")),
                      expression(paste(T^0,sep=" ","baja"))),pch=1:2,col=1:2,bg="cornsilk")
plot(X,rstandard(mrlm2),main="residuos estudentizados vs. X\nModelo de efectos con MRL",
     xlab="X",pch=as.numeric(Temperatura),col=as.numeric(Temperatura),cex=1.5)
abline(h=c(-2,0,2),lty=2)
legend("top",legend=c(expression(paste(T^o,sep=" ","alta",sep=" ","(",sep="",X==+1,sep="",")")),
                      expression(paste(T^o,sep=" ","baja",sep=" ","(",sep="",X==-1,sep="",")"))),pch=1:2,col=1:2,bg="cornsilk")





