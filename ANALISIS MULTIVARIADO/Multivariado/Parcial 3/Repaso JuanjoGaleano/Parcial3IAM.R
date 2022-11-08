bd <- read.table(file.choose(), header = T, sep = " ")
library(MVN)
library(tidyverse)
#EJERCICIO 1
g1 <- bd %>%  filter(grupos == 1)
g2 <- bd %>% 
  filter(grupos == 2)


# i) Probar normalidad cada poblacion

#NORMALIDAD EN CADA POBLACIÓN

#Claramente, hay 9 variables y la decima clmna es
# el grupo. Entonces se realiza prueba de normalidad

# H0: Datos normales
# H1 : Datos NO normales

mvn(data =g1[,-10], mvnTest = "mardia")
mvn(data =g2[,-10], mvnTest = "mardia")

mvn(g1[, -10]) # se vverifica grupo 1 es normal
mvn(g2[, -10]) # se verifica grupo 2 es normal

#(ii)
M_test(list(g1[, -10], g2[, -10]), 0.05) # Prueba de igualdad de matrices
# M_test de Box H0: MatrizSigma1 = MatrizSigma2 = MatrizSigma
# H1: Matrices var-cov diferentes

# Aca NO se rechaza H0 y se concluye matrices de var-cov iguales

#(iii) REalice la prueba pertinente segun resultado del inciso anterior

# COmo matriz var-cov es igual: Se compara si el vector de medias es igual
# para las dos poblaciones cuando sigma es desconocido.
# Sigma se estima con S y se hacen los calculos.

two_mu_sigmaequals_unknown(g1[, -10], g2[, -10], 0.05)
# esta fn :  #difference of means equals to delta0 if varcovs are
#unknow and equals

# Se mete datos1, datos2, nivel de significancia

# H0: Medias las dos poblaciones son iguales
# H1: Medias diferentes

# Aca se concluye que:

#LITERAL (b)
#(i) Listo
#

#(i)
obj <- mvn(g1[, -10]) # Prueba de normalidad para grupo 1

#(ii)
mu0 <- c(5, 5, 9, 7, 5, 5, 1, 2, 4) # Vector mu0 prueba de hipotesis.

# Recuerde que H0: Mu=Mu0 donde Mu0 es la media a comparar.

# Aca se quiere saber si el vector de medias de los datos es igual a 
# el vector mu0 por el que preguntan

mu_nosigma(mu0, g1[, -10], 0.05)

#Function for ht in a multivariate sample with normal distribution when varcov matrix is unknown
# Se usa eta fn cuando se desconoce matriz var-cov sigma.

# Se rechaza H0 y se concluye que son deiferentes por el valor p


#(ii) AHORA ASUMA n=50 DE UNA SOLA POBLACION. HAGA LA SGT PRUEBA

mu0 <- c(1, 9, 4, 7, 5, 5, 9, 3, 3)

#LITERAL (c)
rm(mu0)
mu0 <- c(1, 8, 5, 1, 7, 0, 0, 8, 8) # nuevo vector de media

# Aqui se asume que se tiene una poblacion completa con n>50.
# Un resultado asintotico entonces

mu_nosigma_lct(mu0, bd[, -10], 0.05)

# Basicamente, coja todos los datos y cuando mu=mu0 sin sigma

# Se rechaza H0 a favor Ha y se concluye medias diferentes


#LITERAL (d)
#(i)
mvn(bd[, -10])


#(ii) COntraste de medias, asi se hace

C <- matrix(c(1, 1, 1, 1, -3, -5, -7, -4, -5,
              5, 2, 4, 7, -1, -1, -1, -1, 5), byrow = T, ncol = 9)

gamma <- c(0, 0) # Vector al cual se igualan las pruebas de hipotesis

contrast_sigma_unknown(bd[, -10], C, gamma, 0.05)

# Juego de hipotesis es: 

#H0:  C*mu = gamma
# H1: C*mu != gamma

# Para este caso se rechaza H0 a favor de H1 y se concluye que
# existe evidencia para concluir que c¨mu diferene de gama = 0 (vector nulo)











