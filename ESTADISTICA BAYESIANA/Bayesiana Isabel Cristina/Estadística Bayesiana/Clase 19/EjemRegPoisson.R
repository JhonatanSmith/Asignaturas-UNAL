library(R2OpenBUGS)

modelregpo<- function(){
  # Poisson model likelihood
  for (i in 1:30){ 
    damage[i] ~ dpois( lambda[i] )
    log(lambda[i]) <- beta[1] + beta[2] * type[i] + beta[3] * bombload[i] + beta[4] * airexp[i]
  }
  #
  # prior
  for (j in 1:4){
    beta[j]~dnorm( 0.0, 0.001 )
    B[j] <- exp( beta[j] )
  }
}

inits <- function(){list( beta=c(0,0,0,0))}

data <- list (damage = c(0, 1, 0, 0, 0, 0, 1, 0, 0, 2, 1, 1, 1, 1, 2, 3, 1, 1, 1, 2, 0, 1, 1, 2, 5, 1, 1, 5, 5, 7), 
type = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
bombload = c(4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 7, 7, 7, 10, 10, 10, 12, 12, 12, 8, 8, 8, 14, 14, 14), 
airexp = c(91.5,84,76.5,69,61.5,80,72.5,65,57.5,50,103,95.5,88,80.5,73, 116.1, 100.6, 85, 69.4, 53.9, 112.3, 96.7, 81.1, 65.6, 50, 120, 104.4, 88.9, 73.7, 57.8))

sim.poisson<- bugs(data = data, inits = inits, parameters.to.save = c("beta", "B"), 
                model.file = modelregpo, n.iter = 10000,n.burnin=1000,digits=6)

attach.all(sim.poisson$sims.list)

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
plot(density(beta[,1]),main = expression(paste("Distribución posterior de  ", beta[1], "")),xlab=expression(beta[1]),ylab="Densidad")
plot(density(beta[,2]),main = expression(paste("Distribución posterior de  ", beta[2], "")),xlab=expression(beta[2]),ylab="Densidad")
plot(density(beta[,3]),main = expression(paste("Distribución posterior de  ", beta[3], "")),xlab=expression(beta[3]),ylab="Densidad")
plot(density(beta[,4]),main = expression(paste("Distribución posterior de  ", beta[4], "")),xlab=expression(beta[4]),ylab="Densidad")

layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE)) 
plot(beta[,1],type="l",ylab=expression(beta[1]),xlab= "Iteración")
plot(beta[,2],type="l",ylab=expression(beta[2]),xlab= "Iteración")
plot(beta[,3],type="l",ylab=expression(beta[3]),xlab= "Iteración")
plot(beta[,4],type="l",ylab=expression(beta[3]),xlab= "Iteración")


layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE))
acf(beta[,1],main=expression(beta[1]))
acf(beta[,2],main=expression(beta[2]))
acf(beta[,3],main=expression(beta[3]))
acf(beta[,4],main=expression(beta[4]))



resultados<-data.frame(apply(beta,2,quantile, probs = c(0.025, 0.5, 0.975)))
names(resultados)=c("beta1","beta2","beta3","beta4" )
resultados
resultados1<-data.frame(apply(B,2,quantile, probs = c(0.025, 0.5, 0.975)))
names(resultados1)=c("B1","B2","B3","B4" )
resultados1

