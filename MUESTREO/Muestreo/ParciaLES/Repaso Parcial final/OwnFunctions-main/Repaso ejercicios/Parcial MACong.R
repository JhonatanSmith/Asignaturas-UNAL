# Codigos coste parcial 3

1.

{r}
N=55
Mo=540
Mi=c(14,11,8,17)
ui=c(5.0714,5.2727,6.25,5.7647)
mi=rep(5,4) # Equivalente a c(5 ,5, 5 ,5)
yi=c(5.6,5.2,6.6,5.4)

taui=yi*Mi # Asi obtiene a los totales  (Mi*yi)
si2=c(2.3,4.7,2.3,1.8)

tau_mu_2(taui = taui,Si2 = si2,Mi = Mi,mi = mi,N = N,Mo = Mo)

2.

{r}
taui=c(144.82,144.59,143.51,143.2,146.21,144.51,143.4)
M=9
N=150
B=0.02
D=((B*M)^2)/4


N=150
s2=var(taui)
D=((B*M)^2)/4

con_equals_sample_size(N = N,S2 = s2,D = D)


{r}

3.
M=11
N=145
pi=c(0.454545,0.181818,0.181818,0.454545,0.727273,0.545455,0.545455)
n=7

prop_tot_con(M = M,N = N,n = n,Pi =pi)


{r}
taui=c(142.38,144.41,143.26,143.34,144.46,143.32,143.64)
M=9
N=145
n=7

mu_tau_con(tau_i = taui,M = 9,N = N,n = n)


{r}
Mi=c(length(c(3,8,5,8,6,3,8,5,8,3,4,7)),length(c(5,3,3,3,5,5,5,7,8,7,3,5)),length(c(8,8,5,7,3,6,8,4,6)),length(c(8,6,4,6,7,3,6,6,4,5,5,3,3,4,3,5,8,4)))
taui=c(68,59,55,90)
N=47
Mo=500
tau_muc_mu(Mi = Mi,taui = taui,N = N,Mo = Mo)




{r}
Mi=c(length(c(3,8,5,4,4,4,8,6,8,6,3,3,5,6,3)),length(c(4,3,5,8,6,8,8,7,3,8,3,7)),length(c(4,3,8,8,3,3,6,8)),length(c(4,4,7,6,6,3,5,7,4,8,3,6,7,6,3,6)))
taui=c(76,70,43,85)
N=47
Mo=520
tau_muc_mu_r(Mi = Mi,taui = taui,N = N,Mo = Mo)





{r}
Mi=c(length(c(4,5,8,3,7,5,3,3,5,8,7,8,7)),length(c(4,3,7,8,8,7,6,7,7,3)),length(c(3,6,3,7,5,7,8,8,8)),length(c(5,5,8,3,4,4,5,8,3,5,5,8,3,4,7,6,6)))
N=53
Mo=500
n=4
mi=rep(3,4) # Equivalente a tener c(3,3,3,3)
si2=c(8.3333,4,3,1)
yibar=c(4.6667,6,6,5)


EstRazMu(Mi = Mi,N = N,n = n,mi = mi,si2 = si2,yibar = yibar,M0 = Mo )