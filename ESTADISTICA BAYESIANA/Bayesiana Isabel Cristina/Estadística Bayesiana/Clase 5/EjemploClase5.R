#Numeral a), alternativa 1
library(BayesDA)
data(light)
hist(light,breaks=30)
ybar=mean(light)
s=sd(light)
LS=ybar+qt(0.975,65)*s/sqrt(length(light))
LI=ybar-qt(0.975,65)*s/sqrt(length(light))


#Simulación, alternativa 2
a=rchisq(1000, 65)
b=65*s^2/a
sigma=mean(b)
c=rnorm(1000,26.2,sigma/66)
mean(c)
quantile(c, c(.025, .975)) 

#Numeral b
library(pscl)
mu=25
n=length(light)
nu=sum((light-mu)^2)/n
alpha=0.1
beta=0.2
nu0=2*alpha
sigma0=2*beta/nu0
a2=(n+nu0)/2
b2=(n*nu+nu0*sigma0)/2
qigamma(0.025,a2,b2)
qigamma(0.975,a2,b2)

#Inferencia sobre media y varianza, clase 5
nun=nu0+n
k0=5
mu0=mu
uno=n*k0*(ybar-mu0)^2/(n+k0)
sigman=(nu0*sigma0+(n-1)*s^2+uno)/(n+nu0)
mun=(mu0*k0+n*ybar)/(n+k0)

#Para sigma
qigamma(0.975,(nun/2),(nun*sigman/2))
qigamma(0.025,(nun/2),(nun*sigman/2))

#Para theta
theta=rt(1000,nun)*sqrt(sigman/(n+k0))+mun
quantile(theta, probs = c(0.025, 0.975))


#Gráficas para la media
mediaa<-ybar+rt(1000,(n-1))*s/sqrt(n)
mediab<-theta
plot(density(mediaa),main="",xlab = expression(mu),ylab="Densidad")
lines(density(mediab),col=2)

#Gráficas para la varianza
varianzaa<-rigamma(1000,a2,b2)
varianzab<-rigamma(1000,(nun/2),(nun*sigman/2))
plot(density(varianzaa),main="",xlab = expression(sigma^2),ylab="Densidad")
lines(density(varianzab),col=2)

