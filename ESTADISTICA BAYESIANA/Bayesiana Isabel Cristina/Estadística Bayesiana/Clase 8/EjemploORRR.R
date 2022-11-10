a0=a1=b0=b1=1

y1=25
n1=300
y0=30
n0=900
pi0=rbeta(1000,y0+a0,n0-y0+b0)
pi1=rbeta(1000,y1+a1,n1-y1+b1)
RR<-pi1/pi0
mean(RR)
plot(density(RR), xlab='Riesgo relativo',ylab='Densidad posterior',main='')
OR<-pi1*(1-pi0)/(pi0*(1-pi1))
mean(OR)
plot(density(OR), xlab='Razón de odds',ylab='Densidad posterior',main='')
