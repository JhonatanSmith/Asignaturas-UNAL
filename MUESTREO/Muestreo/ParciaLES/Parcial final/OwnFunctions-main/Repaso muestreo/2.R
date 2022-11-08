N = 415 # Bloques
n = 15

tau_i = c(96,121,42,65,52,40,75,65,45,50,85,43,54,49,53,50,32,22,45,37,51,30,39,47,41)  # Total por conglomerado

mi = c(8,12,4,5,6,6,7,5,8,3,2,6,5,10,9,3,6,5,5,4,6,8,7,3,8)

M0bar =  (sum(mi)/25)*N

M0 = N*mean(mi)

M0 = 25*sum(mi)

tau_muc_mu(mi, 1000*tau_i, N, M0) 

tau_muc_mu_r(mi, 1000*tau_i, N, M0)

# Taller 4C CONGLOMERADOS

2.

n = 20 
N = 96 
M_i = c(3,7,11,9,2,12,14,3,5,9,8,6,3,2,1,4,12,6,5,8)
taui = c(50,110,230,140,60,280,240,45,60,230,140,130,70,50,10,60,280,150,110,120)

a) Establezca costo promedio de reparacion. 

M0 = N*mean(M_i) # Estimacion de M0 cuando es desconocido

tau_muc_mu_r(M_i, taui, N, M0) # Se usa razon cuando M0 desconocido

mu = 19.73077 # Promedio por unidad de muestreo 
Tau = 12312.00000 # Total poblacional
Bmu = 1.780103
Btau = 1110.784504

# Taller 4B  REVISAR

2.

N=50

Mi=c(52,56,60,46,49,51,50,61,60,45)

Mo=mean(mi)*N # Se estima ese hpta M0

#Mo = m_bar*N

mi=c(5,6,6,5,5,5,5,6,6,6)


# Codigo inventado por coste

conglomerados=list(c(12,11,12,10,13),c(10,9,7,9,8,10),c(6,5,7,5,6,4),c(7,8,7,7,6),
                   c(10,11,13,12,12),c(14, 15, 13, 12,13),c(6,7,6,8,7),c(9,10,8,9,9,10),
                   c(7,10,8,9,9,10),c(12,11,12,13,12,12))

# Primero esta
a= first(conglomerados, Mi) 

tau_mu_2(a$taui, a$Si2 , Mi, mi, N, Mo)
# Luegp esta
  
Mo=mean(Mi)*N  
  
tau_mu_2(conglomerados, coste$Si2, Mi, mi, N, Mo)

3. Ejercicio 

M0 = 2600

# Aplique first que (coste)

tau_mu_2(coste$taui, coste$Si2, Mi, mi, N, M0)

