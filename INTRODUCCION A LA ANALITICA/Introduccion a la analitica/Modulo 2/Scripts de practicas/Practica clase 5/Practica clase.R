# Ridge Regression, Lasso o REGULARIZACION:

Cuando en un proceso se tienen p variables y n observaciones PERO p>n; es decier,
el numero de variables es mayor que el de observaciones, se recomienda utilizar
regresion Ridge/LASSO.

Esto es conocido como una regularizacion de los datos, donde allí se va a 
tener a todas las variables en la misma escala.

 
 * BASICAMENTE, DE MUCHAS VARIABLES, LAS QUE ESTEN MUY RELACIONADAS ENTRE SI SE MIRA
CUALES DESPUES DEL PROCESO DE REGULARIZACION "SOBREVIVEN". Esto es de alguna manera, 
una seleccion de variables.



 *Ridge* hace que betas de muchas de las variables se vayan a cero.

 *Lasso*coge todas las variables y descarta las que tengan multicolinealidad


Realmente no hay diferencia entre ridge y lasso. Mire cual le funciona 
bien y compare a version



# Seleccion del valor de Lamda (hiperparametro)

En general, lambda es desconocido entonces usted debe de hallar metodologias
para identificar cual es el mejor lambda. 

Una mala seleccion de lambda puede causar que todos los parametros se anulen o 
incluso que simplemente haga una RLM. asi que OJO!

1) Se dan muchos valores de lambda
2) Se observa cual de todos tiene el menor error
3) Se utiliza validacion cruzada para hallar el mejor lambda

# EJEMPLO:

require(ISLR)
require(glmnet)

Hitters = na.omit(Hitters)
x= model.matrix(Salary~., Hitters)[,-1] # Asi se saca la matriz de diseño. Se quita columna variable salario
y = Hitters$Salary # Vector de vble respuesta

Se aplicará una funcion del maquete glmnet pero para esto necesito
matriz de diseño y vector de respuestas:
  
gridz = 10^seq(-1,10, length = 100) # Seleccion aleatoria del valor de lambda

#Acá simplemente se realizó un proceso donde selecciono valores aleatorios muy pequeños y muy grandes para lambda
#para asi intentar ver cuales son los mejores.
 
ridge.mod = glmnet(x,y,alpha = 0, lambda = gridz) # Modelo ridge

 OJO! Alpha = 0 significa que haga ridge regression
 
dim(coef(ridge.mod)) 

Hay 20 filas por 100 columnas. Son en total 20 betas (19 vble+ intercepto) 

Y para cada valor de lamba estima los parametros. En esste caso 100 porque sse hicieron 100 lambdas

# Analisis grafico:

plot(ridge.mod, xvar = "lambda", label = TRUE)

Interpretacion: OJO! ESTO ESTÁ EN ESCALA LOGARITMITCA!
  
  En escala logaritmica de datos, tenga las siguientes observaciones:
  
   1) Cuando Lambda tiende a cero, los valores significativos son los de MLE, al mirar
el grafico, se observa que los coeficientes significativos son los 14,19,15, y el otro de la linea negra.
Estos son basicamente betas del modelo de regresion.

   2) Cuando lamba tiende a infinito todos esos valores vana  tender a cero. El truco está
en observar cual de todos los varoes (vbles) se demora en caer y se contrae en el proceso.

Nuevamente, para este caso, se tendra los coeficientes 15,14,19 como los mejores para explicar 
la variablidad del problema plantedo (salario)


# Utilizando validacion cruzada para escoger el mejor valor de lambda

cedula = 1
set.seed(cedula)
train = sample(1:nrow(x), nrow(x)/2) # Conjunto de entrenamiento con proporcion 50%
test = -train # Conjunto de prueba
y.test = y[test] # las y de los datos de entrenamiento


  cv.out = cv.glmnet(x[train,], y[train], alpha = 0) # Esta fn tiene por defecto vc para seleccion de lambda

Analicemos estos datos obenidos...

plot(cv.out) # Grafico salida de la fn anterioir
 

 *Interpretacion* Estos graficos sueltan un MSE que el mismo calcula y te muestra
los resultados del modelo segun el lambda en ESCALA LOGARITMICA. 

Por tanto, si se observa bien, el mejor lambda está cerca de6.1

mejor.lambda = cv.out$lambda.min
mejor.lambda

Entonces el valor de lambda será 431 para los datos de entrenamiento. 


 # Verificando prediccion para modelo con los datos de entrenamiento

ridge.pred = predict(ridge.mod, s = mejor.lambda, newx = x[test,]) # Use el modelo 
# ridge con el mejor lambda calculado y utilice datos de prueba
MSE.predic = mean((ridge.pred-y.test)^2) # MSE del nuevo modelo
MSE.predic

Este es el MSE para el valor de prueba para ridge regression con el lambda calculado

# Seleccion de variables dado ridge regression

out = glmnet(x,y,alpha = 0)
coef.ridge = predict(out,type = "coefficients", s = mejor.lambda)[1:20, ]
coef.ridge

De aqui, se seleccionan los modelos que tuvieron valores mas pequeños pero en valor absoluto. 

En este caso, se eliminan las varuables assists y CAtBat


# Tecnica LASSO:

require(ISLR)
require(glmnet)

Hitters= na.omit(Hitters)
x=model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

gridz = 10^seq(.2,10,length=100)
lasso.mod = glmnet(x,y,alpha = 1, lambda = gridz) # Alpha = 1 le dice que use LASSO

dim(coef(lasso.mod)) # misma idea

# Lasso no las tira suavemente hacai cero si no que las que NO SIRVEN de una descarta

plot(lasso.mod, xvar ="lambda", label = T)

Aca muchas si se hacen cero. Entonces el le dice, quedese con la verde y roja. 

OJO! ESCALA LOGARITMICA!
  
# Seleccion del mejor lambda usando CV

  cedula = 1
set.seed(cedula)
train= sample(1:nrow(x), nrow(x)/2)
test = -train
y.train = y[test]
cv.out = cv.glmnet(x[train,], y[train], alpha =1)
# Misma idea

plot # Se interpreta igual

mejor.lambda = cv.out$lambda.min
mejor.lambda

en este caso, el mejor lambda es de 9.28 aprox

lasso.pred = predict(lasso.mod, s = mejor.lambda, newx = x[test,])
mean((lasso.pred-y.test)^2) # esto es 111721.7

LOS MSE DE LASSO Y RIDGE SI SE COMPARAN Y ME QUEDO CON EL MENOR MSE


Aca, la tecnica LASSO funciona mejor que la tecnica RIDGE

out = glmnet(x,y,alpha = 1)
lasso.coef = predict(out, type = "coefficients", s = mejor.lambda)[1:20,]
lasso.coef

LASSO DICE DIRECTAMENTE CUAL ELIMINAR DIRECTAMENTE. EL LAS TIRA A CERO.
MIRE ESA TABLA Y MELO.

# Si quiero saber cuales son los coeficientes de las bases de datos a utilizar, pues:

lasso.coef[lasso.coef!=0]
 Estas serán las variables significaticas del modelo





