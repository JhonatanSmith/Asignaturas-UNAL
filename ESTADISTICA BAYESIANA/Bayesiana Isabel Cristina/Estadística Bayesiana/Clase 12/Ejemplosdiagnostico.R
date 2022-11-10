#Ejemplo Simon Newcomb
library(BayesDA)
data(light)
hist(light,breaks=30)
min(light)

#varianzas
a=rchisq(100,65)
sigma=65*var(light)/a

media<-function(sigma){rnorm(1,mean(light),sqrt(sigma/66))}
c=unlist(lapply(sigma,media))

noise1<-function(theta,sd) {
rnorm(66,theta,sd)
}

y=mapply(noise1,c,sqrt(sigma))
dim(y)
min=apply(y,2,min)
hist(min,xlim=c(-60,50),main="Histograma del mínimo",xlab="Mínimo")
abline(v=min(light),col="red")
library(FSA)
perc(min,min(light),"geq")
perc(min,min(light),"leq")

#Ejemplo schools
library(R2OpenBUGS)
data(schools)
schools
J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list ("J", "y", "sigma.y")


inits <- function(){
list(theta=c(0.5,0.1,0.3,0.4,0.7,0.5,0.8,0.5), mu.theta=0.7, tau.theta=1)
}


modelo<- function(){
	for (j in 1:J){
	y[j] ~ dnorm (theta[j], tau.y[j])
	theta[j]<-mu.theta+alpha[j]
	alpha[j]~ dnorm (0, tau.theta)
	tau.y[j] <- pow(sigma.y[j], -2)
	}
	mu.theta ~ dnorm (0.0, 1.0E-6)
	tau.theta~dgamma(0.01,0.01)
	sigma.theta<-1/tau.theta
}


sim.cole<- bugs(data = data, inits = inits, parameters.to.save = c("theta", "mu.theta",
    "sigma.theta"), model.file = modelo, n.chains = 1, n.iter = 10000,n.burnin=1000)


names(sim.cole$sims.list)

thetas=matrix(sim.cole$sims.list$theta,ncol=9000,byrow=T)
dim(thetas)
sdthetas=schools$sd
noise1<-function(theta,sd) {
rnorm(1,theta,sd)
}


x=mapply(noise1,thetas,sdthetas)
xa=matrix(x,ncol=8,byrow=TRUE)
min1=apply(xa,1,min)
max=apply(xa,1,max)
media=apply(xa,1,mean)
mediana=apply(xa,1,median)

hist(max)
abline(v=max(y),col="red")
perc(max,max(y),"geq")


hist(min1)
abline(v=min(y),col="red")
perc(min1,min(y),"geq")

hist(media)
abline(v=mean(y),col="red")
perc(media,mean(y),"geq")

hist(mediana)
abline(v=median(y),col="red")
perc(media,median(y),"geq")

