Profe, mi compañero y yo tenemos muchas dudas respecto a los calculos a realizar en R pues nuestra experiencia con 
el programa es muy reducida. ¿Esto está correcto? Porque nos sentimos como andando muy a ciegas en el trabajo, ademas; no 
sabemos si las simulaciones y procesos que estamos realizando estan correctos. Si nos pudiese ayudar con esto, se lo agradeceriamos.

Segun nosotros, de esta manera podemos calcular los parametros necesarios para resolver el problema. Sin embargo, los 
resultados de los parametros elicitados, uno de ellos es negativo y no sabemos el por que. 

Finalmente, profe; nos matamos la cabeza intentando ver como sacar esos datos simulados y; tenemos dudas acerca de que 
hacer con ellos. En ultimas, damos con un alfa y un beta. Pero, ¿que hacemos con ellos? ¿Son los parametros para una gamma
inversa? Si este es el caso, tenemos problemas pues toma valores negativos.


mu1 = 2.7 # Para moto boxer primer año seleccionado (2018)
sigma1 = 8.1665 # Sigma moto boxer 2018 (Todo está en miles de peso)

mu2 = 3.8  # Moto segundo año seleccionado (2020)
sigma2 =  15.754 # Sigma segundo año seleccionado (2020)

# Estos dos resultados se obtienen de la elicitacion del experto. 

set.seed(2021)  # Una prob fija para que den siempre los mismos valores

moto_2018 = abs(rnorm(1000*20,mu1,sqrt(sigma1))) # Una muestra simulada de 1000 datos a partir de la info elicitada experto moto 1

moto_2020 = abs(rnorm(1000*20,mu2,sqrt(sigma2))) # Muestra simulada 1000x20 datos de moto 2

matriz_2018 = matrix(moto_2018, ncol = 20) # Estos valores, cuando no se le aplicaba la funcion abs a moto_2018 eran negativos.
matriz_2020 = matrix(moto_2020, ncol = 20)

Asumimos que no se deben de tomar valores negativos para los precios elicitados asi que tomamos valor absoluto a la funcion.
Ignoramos si esto se puede o no. En ambos casos, tenemos dudas pues los resultados son extraños pues con o sin el abs, no son
resltados consistentes...


datos =  cbind(matriz_2018,matriz_2020) # Unimos los datos calculados en una sola matriz

# Funcion para estimar los parametros. 

Creamos esta funcion para estimar los parametros y calcular lo pedido. 


library(MCMCpack)
parametros = function(y){
  x = c(rep(2018,20),(rep(2020,20)))
  res = as.vector(summary(MCMCregress(y~x))$statistics[,1]) 
  return(res)
}

# Estimacion de parametros con MCMC

prueba2= apply(datos,1,parametros) # Esto suelta una serie de valores de B0,B1, sigma2

prueba3 = matrix(prueba2, ncol = 3) # Este es una matriz con la que se planea estimar los valores pedidos sacandolos
#como vectores. Para eso lo convertimos en matriz

De aqui en adelante, intentamos simular sus pasos profe en las diapositivas de la clase 19.

colMeans(prueba3[,-3]) # donde se obtiene B0 y B1 En un set.seed(2021) se obtuvo que B0 = -389.3344
# B1 = -413.6469 

solve(var(prueba3[,-3])) # Con esto obtuvimos el grado de precision y fueron valores muy cercanos a cero.

[1,] 1.917868e-06 5.163203e-07
[2,] 5.163203e-07 1.773259e-06

# Parametros para la gamma
m<-mean(prueba3[,3])
v<-var(prueba3[,3])
# Estos son los parametros de la gamma inversa
(alfa<-m^2/v+2) 
(beta<-m*(m^2/v+1))

# Despues de todo este proceso, la gamma inversa está dada por

2.276357 alfa
-544.0774 beta

Como es posible esto? Los parametros no podrian ser negativos y haciendolo de esta forma, los valores
que toman son muy extraños...

Ademas, ¿Que hacemos con ellos? Los metemos en una gamma inversa?


