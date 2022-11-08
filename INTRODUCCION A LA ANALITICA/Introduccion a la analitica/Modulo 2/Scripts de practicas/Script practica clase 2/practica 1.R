library(ISLR)
dim(Auto) # Hay 392 registros y 9 variables
names(Auto) # El nombre de las variables

attach(Auto)

cedula = 33234 # Una semilla de aleatorizacion

set.seed(cedula)

# Hay 392 datos, osea que la mitad son 196

subset1 = sample((1:nrow(Auto)),196) # Selecciono 196 datos aleatorios

Train1 = Auto[subset1,] # Datos de entrenamiento

Test1 = Auto[-subset1,] # Datps de prueba

# La idea es ver la relacion entre mpg (millas por galon) y horsepower
# Caballos de fuerza

library(ggplot2)
require(ggplot2)
ggplot()+
  geom_point(data=Auto, aes(horsepower, mpg), color = "blue", size=2)

Claramente se ve una relacion entre las variables graficadas

Con este codigo se va a calculoar diferentes MSE de diferentes polinomios
ajustado a esa nube de puntos


MSE = vector()
for (i in 1:10){
  
  Modelo1 = lm(mpg~poly(horsepower,i), data = Train1)
  Pred1 = predict(Modelo1, Test1)
  MSE[i] = mean((Pred1-Test1$mpg)^2) #Promedio de lo predecido menos lo real
  
}

plot(1:10, MSE, xlab = "Grado polinomio ajustado", ylab = "MSE", type = "b", col=4)

Claramente el mejor MSE es el grado de polinomio de grado 2.Esta divisiom de los datos tambien 
puede representar diferencas.

EN GENERAL, DATOS GRANDES SI TRABAJA. NO CIN DATOS PEQUEÑOS

SOBREAJUSTAR: Crear una cosa tan perfecta para mis datos que cuando se vaya a probar
para predecir datos diferente no sirva para nada ese modelo


# Leave-One-Out Cross validation (LOOCV)

Saque un solo dato de toda la base de datos. Luego, toda la base de datos
menos ese punto será el conjunto de entrenamiento.

Ya con esto, utilice ese modelo y lo prueba en su dato que sacó.


Repita el anterior proceso con una observacion 2. Y luego evalue en el dato 2.

Y asi se van a realizar n observaciones y se van a sacar n modelos.

















