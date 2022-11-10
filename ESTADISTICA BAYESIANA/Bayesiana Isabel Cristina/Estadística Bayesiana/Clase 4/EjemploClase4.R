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

