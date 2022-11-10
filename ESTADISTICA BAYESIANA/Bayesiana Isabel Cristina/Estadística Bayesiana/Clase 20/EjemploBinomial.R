library(R2OpenBUGS)

modelregbino<- function(){
#Verosimilitud
    for (i in 1:n){ 
    senility[i] ~ dbern( pi[i] )
    logit( pi[i] ) <- beta0 + beta1 * wais[i]
  }
  # Distribuciones a prior 
  beta0~dnorm( 0, 0.001)
  beta1~dnorm( 0, 0.001)
  #
  odds0 <- exp(beta0)
  OR <- exp(beta1)
  #
  # Wais para el cual pi=1/2
  wais.half.prob <-  - beta0/beta1 
  

}

inits <- function(){list( beta0=0, beta1=0.01 )}

data<-list(n=54, wais=c(9,13,6,8,10,4,14,8,11,7,9,7,5,14,13,16,10,12,11,14,15,18,7,16,9,
                        9,11,13,15,13,10,11,6,17,14,19,9,11,14,10,16,10,16,14,13,13,9,15,
                        10,11,12,4,14,20),senility=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0,0,0))

sim.binomial<-bugs(data=data,inits=inits,parameters.to.save=c("beta0","beta1", "odds0","OR",
                  "wais.half.prob"),model.file=modelregbino, n.iter=10000,n.burnin=1000)

attach.all(sim.binomial$sims.list)

quantile(beta0,probs = c(0.025, 0.5, 0.975))
quantile(beta1,probs = c(0.025, 0.5, 0.975))
quantile(odds0,probs = c(0.025, 0.5, 0.975))
quantile(OR,probs = c(0.025, 0.5, 0.975))
quantile(wais.half.prob,probs = c(0.025, 0.5, 0.975))
