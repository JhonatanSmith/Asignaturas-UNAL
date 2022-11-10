library(R2OpenBUGS)

tiempo = c(16.68,11.5,12.03,14.88,13.75,18.11,8,17.83,79.24,21.5,40.33,
           21,13.5,19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75) 
distancia=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776, 
            200,132,36,770,140,810,450,635,150)
casos=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
datos1<-data.frame(tiempo,distancia,casos)
plot(datos1)

modregres<- function(){
    for (i in 1:n){
		     tiempo[i] ~ dnorm( mu[i], tau ) 
		     mu[i] <- beta0 + beta1 * casos[i] + beta2 * distancia[i]   
		     }
		# Distribuciones a priori
		tau ~ dgamma( 0.01, 0.01 )
    beta0 ~ dnorm( 0.0, 1.0E-4)
    beta1 ~ dnorm( 0.0, 1.0E-4)
    beta2 ~ dnorm( 0.0, 1.0E-4)
		# Definición de sigma
		s2<-1/tau
		s <-sqrt(s2)
		# residuals
		for ( i in 1:n ){
		  r[i]     <- (tiempo[i] - mu[i])
      }
}
#Datos
datos<-list(n=25,tiempo = c(16.68,11.5,12.03,14.88,13.75,18.11,8,17.83,79.24,21.5,40.33,
            21,13.5,19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75), 
            distancia=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776, 
            200,132,36,770,140,810,450,635,150),casos=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6, 
		9,10,6,7,3,17,10,26,9,8,4) )

#Valores iniciales
regresinits <- function() {list( tau=1, beta0=1, beta1=0, beta2=0 )}

sim.regres <- bugs(data = datos, inits = regresinits, parameters.to.save = c("beta0", 
    "beta1", "beta2","s2","r"), model.file = modregres, n.chains = 1, n.iter = 10000,n.burnin=1000,
    n.thin=1,)


attach.all(sim.regres$sims.list)

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
plot(density(beta0),main = expression(paste("Distribución posterior de  ", beta[0], "")),xlab=expression(beta[0]),ylab="Densidad")
plot(density(beta1),main = expression(paste("Distribución posterior de  ", beta[1], "")),xlab=expression(beta[1]),ylab="Densidad")
plot(density(beta2),main = expression(paste("Distribución posterior de  ", beta[2], "")),xlab=expression(beta[2]),ylab="Densidad")
plot(density(s2),main = expression(paste("Distribución posterior de  ", sigma^2, "")),xlab=expression(sigma^2),ylab="Densidad")

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE)) 
plot(beta0,type="l",ylab=expression(beta[0]),xlab= "Iteración")
plot(beta1,type="l",ylab=expression(beta[1]),xlab= "Iteración")
plot(beta2,type="l",ylab=expression(beta[2]),xlab= "Iteración")
plot(s2,type="l",ylab=expression(sigma^2),xlab= "Iteración")

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
acf(beta0,main=expression(beta[0]))
acf(beta1,main=expression(beta[1]))
acf(beta2,main=expression(beta[2]))
acf(s2,,main=expression(sigma^2))



residuales<-apply(r,2,mean)
yhat<-mean(beta0)+mean(beta1)*casos+mean(beta2)*distancia
normresidual<-residuales/sqrt(mean(s2))

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
plot(datos$distancia,normresidual,ylab="residuales",xlab="Distancia",
     ylim=c(-3,3))
abline(h=c(-3,0,3), col="red")
plot(datos$casos,normresidual,ylab="residuales",xlab="Casos",
     ylim=c(-3,3))
abline(h=c(-3,0,3), col="red")
plot(yhat,normresidual,,ylab="residuales",xlab="Tiempo ajustado",
     ylim=c(-3,3))
abline(h=c(-3,0,3), col="red")

qqnorm(residuales)
qqline(residuales)

a<-c(quantile(beta0,probs=c(0.025,0.5,0.975)))
b<-c(quantile(beta1,probs=c(0.025,0.5,0.975)))
c<-c(quantile(beta2,probs=c(0.025,0.5,0.975)))
resultados<-data.frame (cbind(a,b,c))
beta <- intToUtf8(0x03B2) 
a <- 0:2
names(resultados) <- paste0(beta, a)
resultados
