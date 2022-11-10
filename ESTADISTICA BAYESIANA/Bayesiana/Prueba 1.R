mu1 = 2.63 # Para moto boxer 2019 es este 
sigma1 = 7.29538 # Sigma moto boxer 2019

mu2 = 3.87  # Moto 2021
sigma2 =  15.504

moto_2019 = rnorm(1000*20,mu1,sqrt(sigma1))

vector_precio_2019 # vector precio
año_2019 = rep(2019,20)
plot()

library(MCMCpack)
  
moto_2021 = rnorm(1000*20,mu2,sqrt(sigma2))

matriz_2019 = matrix(moto_2019, ncol = 20)
matriz_2021 = matrix(moto_2021, ncol = 20)

unidas =  cbind(matriz_2019,matriz_2021)

dim(unidas)

MCMCregress()

# año-kilometros (miles)-precio millones

datos<-scan()
2019 15 1.8
2021 4 4.8
2020 6 3.5
2015 25 2.4
2017 18 2.9
2019 29 2.5
2021 9 4
2016 18 2.3
2014 40 1.4
2012 55 1
2021 0.8 5.5
2020 1.5 5
2021 1 5
2018 4 3.8
2019 11 3.5
2020 25 3.3
2019 25 2.9
2021 15 4

datos<-matrix(datos,ncol=3,byrow=T)

Precio<-datos[,3]
kms<-datos[,2]
Año<-datos[,1]
plot(kms,Precio)
plot(Año,Precio)

library(MCMCpack)

res.bay<-MCMCregress(Precio~kms)

summary(res.bay)

dim(res.bay)

# Estimacion de parametros apriori

# Estimaci´on Normal Multivariable para beta0 y beta1


# Media b0
colMeans(res.bay[,-3])

var(res.bay[,-3])

# Presicion B0
solve(var(res.bay[,-3]))

# Estimacion de parametros para Gamma INversa 
# MM como una apriori sigma^2

m<-mean(res.bay[,3])
v<-var(res.bay[,3])
(alfa<-m^2/v+2)

(beta<-m*(m^2/v+1))

# Se tiene el alpha y beta para una gamma

rnorm()

rgamma(20,alfa,beta)




