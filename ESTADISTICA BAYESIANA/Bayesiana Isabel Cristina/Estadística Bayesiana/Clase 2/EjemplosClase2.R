#Ejemplo Binomial
qbeta(0.025,438,544)
qbeta(0.975,438,544)

#Ejemplo Poisson 
y<-c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22)
sum(y)
#Parámetros a priori
alpha<-101
beta<-5
#Parámetros posterior
alpha_posterior<-sum(y)+alpha
beta_posterior<-length(y)+beta
thetaposterior=rgamma(1000,alpha_posterior,beta_posterior)
mean(thetaposterior)
sd(thetaposterior)
qpois(0.5,mean(thetaposterior))

#Otra alternativa
library(MCMCpack)
posterior <- MCpoissongamma(y, alpha, beta, 5000)
summary(posterior)


