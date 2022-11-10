Las muestras son las 3, 65, 60, 24, 105, 136, 67, 9, 95, 69


# Datos ejercicio 

1. 

prom_muestra = c(1.05,0.88,0.81,0.59,1,1.01,0.74,0.85,
                 1.18,1.14)

n = length(prom_muestra)

prop_muestra = c(1,0,1,1,1,1,1,0,1,1)

prop_si_hijos = c(8)

N= 200

B = 0.06

sigma2 = var(prom_muestra)

alpha = 0.01

sample_size(N, B, sigma2, alpha, total = F)

tamaño muestra es de 49.


# ejercicio 2.

promedio_muestra = c(0.97,0.77,1.02,0.93,1.12,0.72,1.38,0.98,
                    1.07, 0.88,1.11,1.07,1.12,0.89,1.19,
                    1.18,0.77,0.93,0.89,0.88,1.05,0.92,0.98,
                    1.14,1.14,0.94,1.04,0.75,1.2,1.27,0.81,
                    1.25,1.01)

proporcion = c(1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,
               0,1,1,1,1,1,0,1,1,1,1,1)

n = 33

a = 26 # Los que cumplen que si tienen hijos

samples(promedio_muestra, n)

mu =1.01121212121212

s2 = var(promedio_muestra)

N = 200

# Con esto se halla IC. 

# Datos, son los datos de la muestra.

confint_mu_tau(datos =promedio_muestra, mu = muL, s2 = s2, n, N, alpha=0.01) 


base = data.frame(matrix(c(promedio_muestra,proporcion),ncol = 2))
attach(base)

base_niños = (base[base$X2==1,])
# C. Gasto promedio  niños

base_niños = (base[base$X2==1,])
attach(base_niños)
prom_kids = base_niños$X1

# Promedio hogares niños 

samples(prom_kids, n=26)

# La varianza será el valor de expecteds2

a = mean(prom_kids) # es 1.060769

n = 26

N1 = 145

#s2_niños = var(base_niños$X1) # es lo mismo

s2_niños = 0.01924738

confint_mu_tau(datos =prom_kids, mu = a, s2 = s2_niños, n=26, N1, alpha=0.01) 

# rta oficial = ( 0.992063 , 1.129475 ) 

# Promedio hogares no niños 

base_no_niños = (base[base$X2==0,])
attach(base_no_niños)
prom_no_kids = base_no_niños$X1
attach(prom_no_kids)
b = mean(prom_no_kids) # es 0.8271429 aprox 0.83

n = 33

N2 = 55

s2 = var(base_no_niños$X1)

# OJO AL REEMPLAZAR DATOS. AKI VAN LOS DATOS DE LA OTRA MUESTRA
# A PARTE SE SACO TODO

confint_mu_tau(datos = prom_no_kids, mu = b, s2 = s2, n=7, N2, alpha=0.01) 

# Ejercicio 4.

Para la poblacion de estudio, se quiere investigar proporcion
de hogares con niños. 

# Con niños.

N = 200
n = 33

ai = sum(base$X2)
p =ai/n # Proporcion pedida

# EL IC es

confint_p_A(p, n, N, alpha=0.01) # (0.6177777 , 0.9579799 )

A = N*p # 157.5758

# IC para A es (123.5555 , 191.596 )


