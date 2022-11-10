Vamos a responder todo:
  
  1) Se tiene un grafico de dispersion donde se va a revisar las variables y si tienen 
una relacion lineal muy marcada. Entonces vemos algunas vemos una correlacion.

con esto, debo sospechar se x6,x8,x9.

Se revisa multicolinealidad y se ve una relacion casi perfecta lineal. Si son muy altas
se deben dener en cuenta

Se puede dar una impresion (sospecha) de puntos extraños.

 2) El modelo se puede ver ajustado en R con el codigo sgt. Es facil.

Con el modelo, no puedo comparar que variable tiene el efecto mas fuerte (coeficientes vbles)
Habria que estandarizar y mirarlo. 

 3) se debe probar que:
  Ho = bj =0
  H1 = bj =! 0
  
  Es decir, si almenos una de las variables tiene significancia.
  
  Con una significancia del 95% se tiene que bJ = 2,5,6 son significativos para el 
  modelo. Por tanto, para estos casos se rechaza la hipotesis nula.
  
  Para esto, se revisa el summary del modelo; se mira el valor p y tpda la cosa. 
  En el p-value (columna) las que tienen los arsteriscos, son las que se debe sospechar.
  
 4) La significancia global del modelo.  

 Ho = B1=B2=...=B9
 H1 = Algun Bj =! 0
 
 H0 implica que globalmente el modelo no es significativo.
 H1 sies significativo. 
 
 Esto se hace con el ANOVA donde el P-valor es de 2.2e-15<alfa (0.05) por tanto el 
 modelo de regresion es globalmente significativo y existe almenos un beta que tiene
 significancia para el modelo. 
 
 Almenos una de las covariables ayuda a explicar el problema real planteado (en este caso, total
crimenes ). almenos una explica la variable Y de respuesta, total crimenes serios. 
 
 5) Calcular el R^2: Se revisa en el summary del modelo (se llama multiple-R-squared)
  
  Se interpreta como el 0.9662 (96%) de la variabilidad de Y: Total crimenes serios 
es explicado por el modelo.

OJO= A mayor numero de covariables este valor es mas alto. No se puede evaluar solo 
esta medida.

OJO= No penaliza el numero de parametro

OJO = R cuadrado alto no implica que se cumplan los supuestos basicos (normalidad),
no es util para predecir. 

 6) Se corre las graficas y se mira que se peude decir de ellas:
  
  *Sobre residuos estudentizados, alrededor del cero* y mirar si hay algun patron en los 
graficos. COmo no hay un patron especifico entonces decimos que no hay problemas de variables
constantes.

Uno querria que se distribuyan en forma de un rectangulo alrededor del cero.

Recuerde que todo deberia estar dentro de mas o menos 3. Los que esten por fuera de ese valor
se deberia de sospechar de ser un valor atipico.

Se podria mirar si hay puntos raros.

Los puntos alejados del eje x (valor de la covariable).

CASO DE PROBLEMAS DE VARIANZA: si hay problemas de varianza, ya no se podria concluuir y seguir con el
analisis

segun profe : no hay probelmas varianza y se ven algunos datos atpicos. Alejados de +-3 y muy alejados
del eje x

 7)  H0 = Se distribuyen normal los errores
     H1 = No se distribuye
     

GRAFICA DE Q-NORM: En la grafica se ve terrible y los puntos deberias coincidir bastane. Al final, los puntos
no cumplen el supuesto de normalidad. 

Segun shapiro wilk, P-value es casi cero, se rechaza hipotesis nula. Entonces se ve que no se distribuyen normales

 8) Vamos a estandarizar los coeficientes de los datos, para tenerlos a todos en la misma magnitud
y asi mirar cual, en su valor absoluto; tiene mayor influecia sobre los datos:
  
  miscoeficientes(modelo,datos[-1]) nos fijamos en los coef.std y ya asi quedan con la misma escala.

En este caso, se ve que x2 es el valor que mas influye es X2

En orden de importancia, tendriamos que (todo esto en valor absoluto):
  
  X2,X8,X6,X5,X9,X3,X1,X7,X4

Pero de aqui, el efecto parcial de cada variable es en ese orden y nos interesa en particular las de
x6,x8,x9 pues son las que inicialmente me hicieron sospechar. 

 9) Observaciones atipicas, de balanceo e  influenciables.

Estas son las observaciones influenciables (tabla de TRUE y FALSE) = 1,2,3,6,9,11,33,43

Las de balanceo, en el HAT y valores mas grandes a 2*(k+1)/n
Para este caso, los valores son 2*(10)/60=0.333 y las de balanceo seran 

1,2,3,6,9,11

Observaciones atipicas: se miran en diagnostics plot (las dos barras del medio) y miramos los valores atipicps

6 y 43. Estas son observaciones atipicas

  10)  miscoeficientes(modelo,datos[-1]) Se usa este codigo

Verifiquemos si hay problemas de multicolinealidad.
Se saca con la funcion mis coeficientes y se mira los VIF´s. Hay problema si el valores >10

Y como el VIF toma valores mayores de 10, nos muestra que hay problemas de multicolinealidad. Aqui no nos dice quien con quien.
Dichos valores son:
  (306.12,20.23,11.59,271.11,153.13)

De acuerdo al indice de condicion hay problemas de multi colinealidad es cuando el valor 
es mayor de 31. Mirando en esta tabla y esto se revisa con el ultimo.

Indice de condicion es mayor que 31, hay problemas de multicolinealidad.

Cuando el valor de la varianza es mayor a 0.5 decimos que hay problema de multicolinealidad.
Se va mirando por fila, cual supera 0.5

Hay problemas serios entre x2 y x8. 

# Suma de cuadrados extras

Mide reduccion marginal de la suma cuadrados del error cuando una o mas covariables
son agregadas al modelo de regresion, dado que hay otras covariables en el
modelo o bien;

Al meter mas covariables hay una reduccion de sumas de cuadrados del error

Pero si esto pasa, aumenta la suma de cuadrados de la regresion.

Se analiza cual de estos dosuamentos es significativos.

Por tanto, es una medida del incremento marginal en la suma de cuadrados de la regresion
cuando una o mas predictoras son agregadas al modelo

PAra este caso tenemos un MRLM con k predictoras; por tanto el modelo full (MF) tiene
todas posibles covariables que se pueden incluir (que tengan sentido)

yi = b0+b1x1+...+bkxik+ ei donde el ei se distribuye n(0,s2h)

y la SS (Sumas de cuadrados) como:
  
  1)  Suma de cuadrados de la regresion (SSR) por notacion (x1,x2,..,xk) con k
grados de libertad (g.l)

2) Sumas de cuadrdos del error (SSE) (x1,x2...,xk) con n-k-1 grados de libertad (g.l)

# Enunciado general:

Para un conjunto de r covariables donde r<k, tenemos xi1,xi2,..,xir, la suma de cuadrados extra es

SSR (xi1,xi2,...,xir|xj=!xi1,xi2,...,xir) se lee que voy a incluir las primeras r dado que en el modelo
estaba inckuidas todas las demas.

Entonces...

SSR(xi:xk)-SSR(x1:xr) 

La primera parte es la suma de cuadrados de la regresion del modelo full

La segunda es Suma de cuadrados de la regresion del modelo reducido

Tambien se puede escribir como

SSE(x1:xr)-SSE(x1:xk)

Donde primera parte es Suma cuad error del MRdeucido

Segunda es Suma cuad modelo full

# Grados de libertad suma cuadrados extra

g.l[SSR(x1,..xr|x1,...,xr)]= k - (k+r)= r

El grado de libertad será r para suma de cuadrados extras

# Ejemplo teorico:

Suponga que k=3 entonces el MF será

yi = b0+b1x1+b2x2+b3x3 + ei donde ei~N(0,s2h)

1) Se desea suma de cuadrados extras tq

SSR(X1|X2,X3) por tanto, el modelo reducido MR será

yi = b0+b2x2+b3x3+ei donde ei~N(0,s2h)


SSR(x1|x2,x3) = SSR(x1,x2,x3)-SSR(x2,x3)

= SS totales depende de su variable Y. 

SSregresion y SSdel error explica que tanta de la variabilidad la esta explicando el modelo


SSdel error explica que tanta de la variabilidad que hay, no la esta explicando el modelo

g.l(3-2)=g.l(1)

# 2: Como seria SSR(x1|x2)


MR <- yi = b0+b2x2+ei donde ei bla bla

MF <- yi = b0+b1x1+b2x2+ ei donde ei~N(0,s2h)

Ahora, SSR(x1|x2)= SSR(x1,x2)-SSR(x2)

Tambien se podria hacer = SSE(X2)-SSE(X1,X1)


gl (2-1)=gl(1)

# Descomposicion de la SSR de un modelo en sumas de cuadrados extras

Nuevamente k = 3

SSR(x1,x2,x3)= SSR(x1)+SSR(x2|x1)+SSR(x3|x1,x2)

Se reescribe de la formula anterior dada

=SSR(x1)+SSR(x2,x1)-SSR(x1)+SSR(x1,x2,x3)-SSR(x1,x2)


= SSR(x1,x2,x3) #En efecto, se puede descomponer


#PARA EL TRABAJO:

SS1 (Suma de cuadrados tipo 1) y son secuenciales:
  
  Son sumas de cuadrados extras de 1 grado de libtertad, 
donde cada covariable es agregada secuecnailmente (lo acabamos de hacer)

Vamos a tener una tabla como esta

Fuente       SS
x1           SSR(x1)->y~x1
x2|x1        SSR(x2|x1)-> Muestra el ss extra de incluir x2 dado que ya estaba x1
x3|x2,x1     SSR(x3|x1,x2) -> asi sucesivamente.


OJO:  se hace en r con la funcion "anova(modelo)"


SS2 (suma de cuadrados tipo 2)-> Parciales. 
Mira el efecto parcial de incluir cada variable. Para k=3

Fuente        SS                g.l
x1|x2,x3      SSR(x1|x2,x3)      1
x2|x1,x3      SSR(x2|x1,x3)      1
x3|x1,x2      SSR(x3|x1,x2)      1

De arriba a abajo, digamos que son caso 1,2 y 3

Para el caso 1:
  
  F0 = SSR(x1|x2,x3)/MSE(x1,x2,x3) ~f1,n-(k+1) este numerador dice que:
  
  (Numerador)Que tanto incrementa la suma de cuadrados de la regresion cuando en el modelo esta x2,x3

(Denominador)variabilidad del modelo total (x1,x2,x3)


H0: B1 = 0 y con esto verificamos si es significativo lo que nos aporta al modelo
H1: B1 =! 0 o no es significativo

si F0 > f_{alpha,n-(k+1)} entonces hay evidencia para rechazar H0.

Lo mismo aplica para el caso 2 y 3.

Esto se calcula en R con la funcion "ANOVA(modelo)".

# CONTINUANDO CON EL EJEMPLO ANTERIOR.

# Suma de cuadrados tipo 1 para el ejemplo estara dada por

anova(modelo)

Analysis of Variance Table

Response: Y
Df Sum Sq Mean Sq   F value    Pr(>F)    
X1         1  17864   17864  144.1071 2.428e-16 ***
X2         1 151361  151361 1221.0038 < 2.2e-16 ***
X3         1    434     434    3.5046  0.067052 .  
X4         1    141     141    1.1386  0.291076    
X5         1    922     922    7.4367  0.008791 ** 
X6         1   5786    5786   46.6757 1.099e-08 ***
X7         1    150     150    1.2103  0.276542    
X8         1    347     347    2.8011  0.100446    
X9         1     43      43    0.3442  0.560073    
Residuals 50   6198     124                        
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


SSR(X1) = 17864 y~x1
SSR(x2|x1) = 152361, es el incremento de SSR al ingresar X2(Poblacion total)
Entonces es incremento al ingresar total crimenes serios vs total poblacional.

y asi sucesivamente. 

La columna de Sq es la de interes, las otras no importan.


TIPO 2

Calculode las secuenciales con (Anova(modelo))

Anova Table (Type II tests)

Response: Y
Sum Sq Df F value    Pr(>F)    
X1         199.6  1  1.6105 0.2102939    
X2        2000.2  1 16.1353 0.0001982 ***
X3         236.1  1  1.9042 0.1737440    
X4         128.6  1  1.0371 0.3133891    
X5         634.9  1  5.1217 0.0280032 *  
X6        5506.3  1 44.4184 2.009e-08 ***
X7         161.3  1  1.3008 0.2594980    
X8         383.8  1  3.0960 0.0846016 .  
X9          42.7  1  0.3442 0.5600726    
Residuals 6198.2 50                      
---
  
  De la tabla los sq son los SSR para cada x
  
  De la tabla, MSE = residuals/g.l es 6198.2/50 
  
  Esto si sirve para realizar las pruebas de hipotesis, por ejemplo:

H0: B1 = 0
H1: B2 =!0

 F0 = SSR(x1|x2,..,x9)/MSE(x1,x2...,x9)= 199.6/(6198.2/50)= 1.6105
 
 si F0 > f_{alpha,n-(k+1)} entonces hay evidencia para rechazar H0.
 
F0 = 1.6105
 
F1 = qf(0.05,1,50)=0.003971813
 
 Claramente F0>F1, por tanto, no hay evidencia para rechazar H0. 
 
## Otra forma
 
 Con el valor de la tabla.
 
 P-value asociado a x1 es 0.2102939 > alpha=0.05 por tanto no hay evidncia
 para rechazar H0, por tanto la variable x1 no es significativa para el modelo entonces
 no ayuda a explicar el total de crimenes graves
 
 
 
 LAS QUE TIENEN ARSTERISCOS EN LA TABLA, SON SIGNIFICATIVAS PARA EL MODELO
 
 
# Relacion importante a tener en cuenta:
 
 F0j = T0j^2
 
 