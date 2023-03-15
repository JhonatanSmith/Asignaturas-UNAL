#-------------Ley Perks 1
#             Definir la fuerza de mortalidad
muxt.pe1 = function(t,x,pars){
a1 = pars[1]
a2 = pars[2]
a3 = pars[3]
m=(a1+a2*exp(a3*(x+t)))/(1+a2*exp(a3*(x+t)))
return(m)}

#---------Definir tpx 
tpx.pe1 = function(t,x,pars){
a1 = pars[1]
a2 = pars[2]
a3 = pars[3]
g = (1-a1)/a3
v = exp(-a1*t)*((a2*exp(a3*x)+1)/(a2*exp(a3*(x+t))+1))^g
return(v)}

#----------ejemplo de parametros
pars=c(0.00025748259, 0.00002552547, 0.10128397060) 
#----------ejemplo de cálculo
t=20
x=75
(p.20.75 =tpx.pe1(t,x,pars))

require(GoFKernel)
x = 40; n = 3000;
f <- function(t) 1-tpx.pe1(t,x,pars)
Tx = random.function(n, f, lower = 0, upper = 110-x, 
kind = "cumulative")
plot(density(Tx),lwd=2,ylim=c(0,0.05))