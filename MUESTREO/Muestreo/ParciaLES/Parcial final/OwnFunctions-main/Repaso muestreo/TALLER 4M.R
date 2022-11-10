ejercicio 2.

# Conglomerados tamaño defierente

N = 55
n = 20
Mi = c(3,7,11,9,2,12,14,3,5,9,8,6,3,2,1,4,12,6,5,8)

M0 = 550
taui = c(50,110,230,140,60,280,240,45,60,230,140,130,70,50,10,60,280,150,110,120)


tau_muc_mu_r(Mi, taui, N, M0) 

a,b)
# En promedio cuesta 19.73077
# B = 1.780103
# Total 12312.00000 con B = 1110.784504

c)
M0= 710
tau_muc_mu(Mi, taui, N, M0) 

mu = 17.34085 
B = 4.471927 

d) 

B = 2
D = (M0/N)^2*B^2/4
sigma2 =845.5607
sample_size(N, sigma2, D)

#N es la cantidad de conglomerados
#sigma2 es la estimacion de la varianza
#D es B^2Mbar^2/4


Tamaño es aprox 88

Punto 9.

# Proporcion de llantas:

N = 175
M = 4
n= 25

Ai = c(2,4,0,1,2,0,4,1,3,1,2,0,1,1,2,2,4,1,0,0,3,1,2,2,1)
pi = Ai*(1/4)


prop_tot_con(M, N, n, Pi = NULL, Ai) # Este da malo

prop_tot_con(M, N, n, pi, Ai= NULL)



Punto 10.

Mi = c(42,27,38,63,72,12,24,14,32,41)
taui=c(83,62,45,112,96,58,75,58,67,80)
n = 10
N = 48

# Se desconoce a M0 por tanto, se estima como 

Mo = N*mean(Mi)

# Al desconocerse M0 se utilizan estimadores de razon

tau_muc_mu_r(Mi, taui, N, Mo)

# Tau 3532.800000 y B 723.3934195


Punto 11.

N =80+160

Mi= c(12,20,8,14,24,15,10,6,16,8,4,3,12,17,24,30,21,9)
taui=c(40,52,30,36,71,48,39,21,51,32,11,10,33,39,61,37,40,41)

n=18

tau_muc_mu_r(Mi, taui, N, Mo = NULL)

# Promedio x ausencia es de 2.735178 con B=0.4427173


Punto 12.

# Inciso A
n=8
Mi = c(12,14,3,20,12,8,10,6)
taui = c(40,39,12,52,37,33,41,14)
mean(Mi) #10.625
mean(taui) # 33.5


# Residentes por hogar
mu_residentes = sum(taui)/sum(Mi)
mu_residentes # 3.152941

S2_residentes = (taui-Mi*mu_residentes)^2
var_mu_residentes = (1/mean(Mi)^2)*sum(S2_residentes)/((n-1)*n)

B = 2*var_mu_residentes
B #0.1059557

# Inciso B

Mi= c(40,39,12,52,37,33,41,14)
taui = c(58,72,26,98,74,57,76,48)

mean(Mi) # 33.5
mean(taui) # 63.625

mu_habitacion = sum(taui)/sum(Mi) # 1.8992




