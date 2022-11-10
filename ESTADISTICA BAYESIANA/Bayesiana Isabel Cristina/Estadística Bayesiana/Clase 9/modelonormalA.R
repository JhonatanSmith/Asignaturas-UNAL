library(R2OpenBUGS)

#Función definiendo el modelo

modnormal<- function(){
    # Verosimilitud
	for (i in 1:N)
	{ y[i] ~ dnorm( mu, tau )    }
    
    mu ~ dnorm( 0, 0.01 ) # a prior para la media
    tau~ dgamma( 0.01, 0.01 ) # a prior para la precisión
    
    # Varianza
    sigma.squared<-1/tau
    # Desviación estándar
    sigma<-sqrt(sigma.squared)
}

#Datos
datos<-list( N=10, y=c(-1.76, 0.38, 1.23, -0.67, -0.47, -1.36, 1.41,-0.07, -1.23, 2.35) )

#Valores iniciales

normalinits <- function() {list( mu=1, tau=2 )}

sim.normal <- bugs(data = datos, inits = normalinits, 
        parameters.to.save = c("mu","tau", "sigma","sigma.squared"),
        model.file = modnormal, n.chains = 1, n.iter = 10000, 
        n.burnin = 1000, n.thin = 1)

#Resultados
sim.normal

attach.all(sim.normal$sims.list)

par(mfrow=c(2,2)) 
plot(density(mu),main = expression(paste("Distribución posterior de  ", mu, "")),xlab=expression(mu),ylab="Densidad")
plot(density(tau),main = expression(paste("Distribución posterior de  ", tau, "")),xlab=expression(tau),ylab="Densidad")
plot(density(sigma),main = expression(paste("Distribución posterior de  ", sigma, "")),xlab=expression(sigma),ylab="Densidad")
plot(density(sigma.squared),main = expression(paste("Distribución posterior de  ", sigma^2, "")),xlab=expression(sigma^2),ylab="Densidad")

par(mfrow=c(2,2)) 
plot(mu,type="l",ylab=expression(mu))
plot(tau,type="l",ylab=expression(tau))
plot(sigma,type="l",ylab=expression(sigma))
plot(sigma.squared,type="l",ylab=expression(sigma^2))

par(mfrow=c(2,2)) 
acf(mu)
acf(tau)
acf(sigma)
acf(sigam.squared)

detach.all(name = "attach.all")
