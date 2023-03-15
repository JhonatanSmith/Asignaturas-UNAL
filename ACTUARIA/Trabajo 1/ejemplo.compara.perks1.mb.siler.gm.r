# compara perks1, beard-makeham, siler

#-------------Ley Perks 1
muxt.perks1 = function(t,x,pars){
a1 = pars[1]
a2 = pars[2]
a3 = pars[3]
a4 = pars[4]
m=(a1+a2*exp(a3*(x+t)))/(a4*exp(a3*(x+t))+1)
return(m)
}

#---------construir tpx 

tpx.perks1 = function(t,x,pars){
a1 = pars[1]
a2 = pars[2]
a3 = pars[3]
a4 = pars[4]
g = (a1*a4-a2)/(a3*a4)
v = exp(-a1*t)*((a4*exp(a3*(x+t))+1)/(a4*exp(a3*x)+1))^g
return(v)}

pars.p=c(7.130052e-07, 2.005330e-05, 1.123180e-01, 1.982141e-05) 


#-------------Ley Makeham-Beard

muxt.mb = function(t,x,pars){
a = pars[1]; b = pars[2]; C=pars[3]; k =pars[4];
C+( a*exp(b*(x+t))  )/( 1 + k*a*exp(b*(x+t)) )
}

tpx.mb = function(t,x,pars){
a = pars[1]; b = pars[2]; C=pars[3]; k =pars[4];
exp(-C*t)*( (exp(-b*x)  + a*k )/( exp(-b*x)+ a*k*exp(b*t)))^(1/(k*b))
}

pars.mb = c(0.000010936, 0.105864876, 0.000227759, 0.327569880) 


#-------------Ley Siler

#-------fuerza mortalidad
muxt.siler = function(t,x,pars){
a1 =  pars[1];b1 =  pars[2];
a2 =  pars[3];a3 =  pars[4];
b2 =  pars[5]
a1*exp(b1*(x+t))+a2+a3*exp(-b2*(x+t))}
#-------supervivencia
tpx.siler = function(t,x,pars){
a1 =  pars[1];b1 =  pars[2];
a2 =  pars[3];a3 =  pars[4];
b2 =  pars[5]
exp((a1 * exp(b1 * x) * b2 
- a3 * exp(-b2 * x) * b1 
- a2 * t * b1 * b2 - a1 * exp((x + t) * b1) * b2 
+ a3 * exp(-(x + t) * b2) * b1) / b1 / b2)
}


pars.s=c(0.0000990053, 0.0871775254, 0.0003376169,
0.0292152503, 2.9848968334)

#-------------Ley Gompertz-Makeham

muxt.gm = function(t,x,pars){
 pars[1] + pars[2]*pars[3]^(x+t)
}


tpx.gm = function(t,x,pars){
a = pars[1]; b = pars[2]; C = pars[3];
p = exp(-a*t - b*C^x*(C^t-1)/log(C))
return(p)}
#-------------parametros

pars.gm = c(0.004115, 0.0001419,1.0827)

#--------ejemplo
x = 56
tx = seq(0,110-x,0.5)

sup.perks1 = tpx.perks1(tx,x,pars.p)

sup.mb = tpx.mb(tx,x,pars.mb)

sup.siler = tpx.siler(tx,x,pars.s)

sup.gm = tpx.gm(tx,x,pars.gm)

fdp.perks1 = tpx.perks1(tx,x,pars.p)*muxt.perks1(tx,x,pars.p)

fdp.mb = tpx.mb(tx,x,pars.mb)*muxt.mb(tx,x,pars.mb)

fdp.siler = tpx.siler(tx,x,pars.s)*muxt.siler(tx,x,pars.s)

fdp.gm = tpx.gm(tx,x,pars.gm)*muxt.gm(tx,x,pars.gm)

par(mfrow=c(1,2))

plot(tx,sup.perks1,type='l')
lines(tx,sup.mb,col='red')
lines(tx,sup.siler,col='blue')
lines(tx,sup.gm,col='darkorange',lwd=2)

legend( "topright", 
c("perks1", "makeham-beard",
 "siler", "gompertz-makeham"), 
text.col=c("black", "red", "blue", "darkorange") )

plot(tx,fdp.perks1,type='l',ylim=c(0,0.1))
grid (10,10, lty = 6, col = "cornsilk2")
lines(tx,fdp.mb,col='red')
lines(tx,fdp.siler,col='blue')
lines(tx,fdp.gm,col='darkorange',lwd=2)

legend( "topleft", 
c("perks1", "makeham-beard",
 "siler", "gompertz-makeham"), 
text.col=c("black", "red", "blue", "darkorange") )


## right-justifying a set of labels: thanks to Uwe Ligges
x <- 1:5; y1 <- 1/x; y2 <- 2/x
plot(rep(x, 2), c(y1, y2), type = "n", xlab = "x", ylab = "y")
lines(x, y1); lines(x, y2, lty = 2)
temp <- legend("topright", legend = c(" ", " "),
               text.width = strwidth("1,000,000"),
               lty = 1:2, xjust = 1, yjust = 1,
               title = "Line Types")
text(temp$rect$left + temp$rect$w, temp$text$y,
     c("1,000", "1,000,000"), pos = 2)




