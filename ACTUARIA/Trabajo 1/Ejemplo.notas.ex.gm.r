# Ejemplo 2.8.2

muxt.gm = function(t,x,pars){
a = pars[1]
b = pars[2]
C = pars[3]
mx = a + b*C^(x+t)
return(mx)}

tpx.gm = function(t,x,pars){
a = pars[1]
b = pars[2]
C = pars[3]
p = exp(-a*t - b*C^x*(C^t-1)/log(C))
return(p)}

#---hombres
s =0.993742
g = 0.998559
C = 1.084033
a = -log(s)
b = -log(g)*log(C)
pars.h = c(a,b,C)

#---mujeres
s =0.995893
g = 0.998215
C = 1.082736
a = -log(s)
b = -log(g)*log(C)
pars.m = c(a,b,C)

x = 80

#------calculo esperanza,
fn.m = function(t){tpx.gm(t,x,pars.m)}
ex.m = integrate(fn.m,0,110-x)$value

fn.h = function(t){tpx.gm(t,x,pars.h)}
ex.h = integrate(fn.h,0,110-x)$value

#------calculo varianza, 
tpxt.m=function(t){t*tpx.gm(t,x,pars.m)}

(sigma.m = sqrt(2*integrate(tpxt.m,0,110-x)$value-ex.m^2))


tpxt.h=function(t){t*tpx.gm(t,x,pars.h)}

(sigma.h = sqrt(2*integrate(tpxt.h,0,110-x)$value-ex.h^2))

M = cbind(c(ex.m,sigma.m),c(ex.h,sigma.h))
rownames(M) = c("experanza vida","des.est")
colnames(M) = c("hombres","mujeres")
(M)

library(xtable)
print(xtable(M))
