library(MCMCpack)
alpha<-c(728,584,138)
thetas<-rdirichlet(3000, alpha)
dim(thetas)
plot(density(thetas[,1]-thetas[,2]),main="",xlab = expression(theta[1]-theta[2]),
     ylab="Densidad")

