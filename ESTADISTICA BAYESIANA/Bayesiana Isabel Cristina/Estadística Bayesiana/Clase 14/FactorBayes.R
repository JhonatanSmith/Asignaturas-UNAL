a=seq(0.000001, 25,by=0.01)
B21= gamma(29)*gamma(2*a)*gamma(20+a)*gamma(8+a)/(0.0116*gamma(21)*gamma(9)
*(gamma(a))^2*gamma(28+2*a))

plot(a,B21,type="l",ylim=c(0,4.3))
abline(h=4,col="red")
