library(MASS)
library(mvtnorm)
library(MVN)
library(clusterGeneration)  ### package clusterGeneration, para generar MSDP ###

#############################################
#############################################

media1 <- c(0, 0)
#varianza <- matrix(c(2, 0, 0, 2), ncol=2)
sigma1<-genPositiveDefMat("eigen",dim=2)$Sigma      

x <- seq(from=-20, to=20, length.out=60)
y <- seq(from=-20, to=20, length.out=60)

fun1 <- function(x, y) dmvnorm(c(x, y), mean=media1, 
                               sigma=sigma1)
fun1 <- Vectorize(fun1)
z1 <- outer(x, y, fun1)

####################

media2 <- c(0, 0)
#varianza1 <- matrix(c(1.3, 1, 1, 1.3), ncol=2)
sigma2<-genPositiveDefMat("eigen",dim=2)$Sigma      

fun2 <- function(x, y) dmvnorm(c(x, y), mean=media2, 
                               sigma=sigma2)
fun2 <- Vectorize(fun2)
z2 <- outer(x, y, fun2)

##############

media3 <- c(0, 0)
#varianza2 <- matrix(c(1.3, -1, -1, 1.3), ncol=2)
sigma3<-genPositiveDefMat("eigen",dim=2)$Sigma      

fun3 <- function(x, y) dmvnorm(c(x, y), mean=media3, 
                               sigma=sigma3)
fun3 <- Vectorize(fun3)
z3 <- outer(x, y, fun3)

#####################
#####################
(det(sigma1))
(det(sigma2))
(det(sigma3))

par(mfrow=c(1, 3), mar=c(1, 1, 1, 1))
persp(x, y, z1, theta=-10, phi=20, expand=0.8, axes=FALSE,box=F)
persp(x, y, z2, theta=-10, phi=20, expand=0.8, axes=FALSE,box=F)
persp(x, y, z3, theta=-10, phi=20, expand=0.8, axes=FALSE,box=F)



#############################################
#############################################

media1 <- c(0, 0)
sigma1 <- matrix(c(1, 0, 0, 1), ncol=2)
#sigma1<-genPositiveDefMat("eigen",dim=2)$Sigma      

x <- seq(from=-10, to=10, length.out=60)
y <- seq(from=-10, to=10, length.out=60)

fun1 <- function(x, y) dmvnorm(c(x, y), mean=media1, 
                               sigma=sigma1)
fun1 <- Vectorize(fun1)
z1 <- outer(x, y, fun1)

####################

media2 <- c(0, 0)
sigma2 <- matrix(c(2, 0, 0, 2), ncol=2)
#sigma2<-genPositiveDefMat("eigen",dim=2)$Sigma      

fun2 <- function(x, y) dmvnorm(c(x, y), mean=media2, 
                               sigma=sigma2)
fun2 <- Vectorize(fun2)
z2 <- outer(x, y, fun2)

##############

media3 <- c(0, 0)
sigma3 <- matrix(c(3, 0, 0, 3), ncol=2)
#sigma3<-genPositiveDefMat("eigen",dim=2)$Sigma      

fun3 <- function(x, y) dmvnorm(c(x, y), mean=media3, 
                               sigma=sigma3)
fun3 <- Vectorize(fun3)
z3 <- outer(x, y, fun3)

#####################
#####################
(det(sigma1))
(det(sigma2))
(det(sigma3))

par(mfrow=c(1, 3), mar=c(1, 1, 1, 1))
persp(x, y, z1, theta=-10, phi=20, expand=0.8, axes=FALSE,box=F)
persp(x, y, z2, theta=-10, phi=20, expand=0.8, axes=FALSE,box=F)
persp(x, y, z3, theta=-10, phi=20, expand=0.8, axes=FALSE,box=F)





