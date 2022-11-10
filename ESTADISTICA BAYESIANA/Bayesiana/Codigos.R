# Muestreador de Metropolis
# Como muestreadora usaremos una gamma.
# Valor inicial
L0<-1
res<-L0
for(i in 1:100000){
  # genera punto candidato
  Lc<-rgamma(1,L0,scale=1)
  cociente<-4*log(1-exp(-8/Lc))-16*8/Lc-dgamma(Lc,L0,scale=1,log=T)-
    (4*log(1-exp(-8/L0))-16*8/L0-dgamma(L0,Lc,scale=1,log=T))
  cociente<-exp(cociente)
  if(cociente>1){
    L0<-Lc
    res<-c(res,Lc)
  }
  else{
    if(runif(1)<cociente){
      L0<-Lc
      res<-c(res,Lc)
    }
  }
}