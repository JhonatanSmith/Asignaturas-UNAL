nota=c(84,58,100,51,28,89,97,50,76,83,45,42,83,64,47,83,81,83,34,61,
       77, 69,  94, 80, 55, 79)
grupo=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 
4, 4, 4, 4, 4, 4)
boxplot(nota~grupo,xlab="Profesor",ylab="Nota",col="orange",
        border="brown")
medias <- tapply(nota, grupo, mean)
points(medias, col = "red", pch = 15)

library(R2OpenBUGS)

modanounfactor<- function(){
     for (i in 1:n){
			mu[i] <- m + alpha[ grupo[i] ]
			nota[i] ~ dnorm( mu[i], tau )
			residual[i]<-nota[i]-mu[i]
			}
		#Restricciones
		alpha[1] <-  -sum(alpha[2:profes])
		

		# Distribuciones a priori 
		m~dnorm( 0.0, 1.0E-04)
		for (i in 2:profes){ alpha[i]~dnorm(0.0, 1.0E-04)} 
		tau ~dgamma( 0.01, 0.01)
		s <- sqrt(1/tau) # precision 
		# residuals
		for ( i in 1:n ){
		  r[i]     <- (nota[i] - mu[i])
		}

}

#Datos
datos<-list(n=26,profes=4,nota=c(84,58,100,51,28,89,97,50,76,83,45,42,83,64,47,83,81,83,34,61,
77, 69,  94, 80, 55, 79),grupo=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 
4, 4, 4, 4, 4, 4) )

#Valores iniciales
anovainits <- function() {list( m=1.0, alpha=c(NA, 0,0,0), tau=1.0 )}

sim.anova<- bugs(data = datos, inits = anovainits, parameters.to.save = c("alpha", 
    "m","s","tau","r"), model.file = modanounfactor, n.chains = 1, n.iter = 10000,n.burnin=1000,
    n.thin=1)

print(sim.anova)

attach.all(sim.anova$sims.list)

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
plot(density(alpha[,1]),main = expression(paste("Distribución posterior de  ", alpha[1], "")),xlab=expression(alpha[1]),ylab="Densidad")
plot(density(alpha[,2]),main = expression(paste("Distribución posterior de  ", alpha[2], "")),xlab=expression(alpha[2]),ylab="Densidad")
plot(density(alpha[,3]),main = expression(paste("Distribución posterior de  ", alpha[3], "")),xlab=expression(alpha[3]),ylab="Densidad")
plot(density(alpha[,4]),main = expression(paste("Distribución posterior de  ", alpha[4], "")),xlab=expression(alpha[4]),ylab="Densidad")

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE)) 
plot(alpha[,1],type="l",ylab=expression(alpha[1]),xlab= "Iteración")
plot(alpha[,2],type="l",ylab=expression(alpha[2]),xlab= "Iteración")
plot(alpha[,3],type="l",ylab=expression(alpha[3]),xlab= "Iteración")
plot(alpha[,4],type="l",ylab=expression(alpha[3]),xlab= "Iteración")


layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
acf(alpha[,1],main=expression(alpha[1]))
acf(alpha[,2],main=expression(alpha[2]))
acf(alpha[,3],main=expression(alpha[3]))
acf(alpha[,4],main=expression(alpha[4]))

residuales<-apply(r,2,mean)
notahat<-rep(mean(m)+apply(alpha,2,mean),c(6,7,7,6))

normresidual<-residuales/mean(s)

layout(matrix(c(1,1,2,2,0,3,3,0), 2, 4, byrow = TRUE))
plot(grupo,normresidual,ylab="residuales",xlab="Profesor",axes = FALSE,
     ylim=c(-3,3),xlim=c(1,4))
ticks <- c(1  ,  2,  3, 4)
axis(side = 1, at = ticks, labels = ticks)
axis(side = 2)
abline(h=c(-3,0,3), col="red")
plot(notahat,normresidual,ylab="residuales",xlab="Tiempo ajustado",
     ylim=c(-3,3))
abline(h=c(-3,0,3), col="red")

qqnorm(residuales)
qqline(residuales)


resultados<-data.frame(apply(alpha,2,quantile, probs = c(0.025, 0.5, 0.975)))
names(resultados)=c("alpha1","alpha2","alpha3","alpha4" )
resultados

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
sumasacumuladas<-apply(alpha,2,cumsum)
indice<-c(1:9000)
plot(sumasacumuladas[,1]/indice,type='l',xlab='Iteración',ylab=expression(alpha[1]))
plot(sumasacumuladas[,2]/indice,type='l',xlab='Iteración',ylab=expression(alpha[2]))
plot(sumasacumuladas[,3]/indice,type='l',xlab='Iteración',ylab=expression(alpha[3]))
plot(sumasacumuladas[,4]/indice,type='l',xlab='Iteración',ylab=expression(alpha[4]))

