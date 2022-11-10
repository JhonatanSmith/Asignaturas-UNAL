# Empiezo 7:12 AM

Ejercicio 1.

# a)

Nh = c(81,61,141) #Poblacion en cada estrato
N = sum(Nh)
nh = c(19,25,19) # Muestras tomadas para cada estrato
n = sum(nh)
yh = c(18,31,23) # Promedio en cada estrato
ph = c(0.6842105,0.72,0.6315789 ) # Proporcion c/d estrato si ven tv
#en cada estrato (A,B y C) respectivamente
s2h = c(49,49,225)

N = sum(Nh)


p_est = sum((Nh/N)*ph) # P_est

y_bar_est = sum((Nh/N)*yh) # Numero de hras promedio
# Por region

var_yh_bar= (1-nh/Nh)*s2h/nh #Varianza de Y-Barra
# para cada estrato

var_y_est = 1/N^2*sum(Nh^2*var_yh_bar) # Varianza media 
#estratificada

B = 2*sqrt(var_y_est) # Valor pedido.

#Tamaño muestra 

Nh = 

Z = 2
wh_optima_costo_igual = (Nh*sh)/(sum(Nh*sh)) #Neyman
B = 0.07
D = B^2/(Z^2) # D es igual a la varianza de ybar estratificado
wh = wh_optima_costo_igual         

n_optimo = sum(((Nh^2*s2h)/wh))/(N^2*D+sum(Nh*s2h))
n_optimo # 37.63701 (o 38)
asignacion = wh*n_optimo
n = 96
ceiling(asignacion) # 11  6 22


# IC calculado
LI = y_bar_est-B
LS = y_bar_est+B

# Inciso b.

p_est = sum((Nh/N)*ph) # 0.6648708
A_est = N*p_est # 209.4343

qh= 1-ph
var_ph = (1 - nh/Nh)*ph*qh/(nh-1)

var_p_est = 1/N^2*sum(Nh^2*var_ph) # 0.005312749

ee = sqrt(var_p_est)
ee
# LEE de aprox 95%
B = ee*2
B # 0.1457772

# Un IC para p-est

IC_P_est = c(p_est-B,p_est+B)
IC_P_est

# Inciso C: Tamaño muestra

B = 3

Z = qnorm(0.975)
wh_optima_costo_igual = (Nh*sh)/(sum(Nh*sh)) #Neyman
D = B^2/(Z^2) # D es igual a la varianza de ybar estratificado
wh = wh_optima_costo_igual           
           
n_optimo = sum(((Nh^2*s2h)/wh))/(N^2*D+sum(Nh*s2h))
n_optimo # 37.63701 (o 38)
asignacion = wh*n_optimo
ceiling(asignacion) # 11  6 22
         

# Ejercicio 2.

N = 820
n = 40
k = 20
y_bar = 4800
sh = 1045

# No hay evidencia acerca del orden de los elementos
# de la poblacion, por tanto el MSL es equivalente a
# MAS

s2h = sh^2

# Varianza media MAS
var_y_bar_mas = (s2h/n)*(N-n)/(N)

B = 2*sqrt(var_y_bar_mas) # 322.2973

# UN IC de aprox 95% es

y_bar - B
y_bar + B


# Otro

n = 21
nenes = c(1.01,0.98,1.01,1.02,1.19,0.95,1.27,1.11,1.15,0.98,0.63,1.22,1.05)
mean(nenes)

# otro no niños prom

no.kids = c(1.07,0.97,0.88,0.93,0.8,0.99)

# Otro 
Nh = c(108,72,153)
nh = c(27,20,24)
yh = c(20,30,23)
ai = c(20,14,18)
ph = ai/nh
s2h = c(36,36,144)
sh = sqrt(s2h)
N = sum(Nh)
n = sum(nh)
y_bar_est = sum((Nh/N)*yh)





