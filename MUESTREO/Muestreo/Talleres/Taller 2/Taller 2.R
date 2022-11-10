##Estimacion Proporcion Estratificada

Nh = c(65,42,93,25) #Introduzca tamaño c/d Nh
N = sum(Nh)
nh = c(14,9,21,6) #Muestras c/d estrato
ah = c(4,2,8,1) #Poblacion de la muestra que cumple condicion deseada
ph = ah/nh
Pestra = (1/N)*(sum(Nh*ph))

##Calculo del  IC

#Calculo de las varianzas

varianzaPropEnCadaEstrato <- function(Nh, nh, ph){
  qh <- 1-ph
  Var <- (1 - nh/Nh)*ph*qh/(nh-1)
  return(Var)
}

Ahora, para calcular varianza Ph
Varianza_Proporciones = varianzaPropEnCadaEstrato(Nh,nh,ph)
VarPest= (1/N^2)*sum(Nh^2*Varianza_Proporciones)

B = (qnorm(1-0.025))*sqrt(VarPest)

Finalmente el IC de *Aproximadamente* 95% Esta dado por:
  
  LimInf = Pestra-B
  LimSup = Pestra+B
  C(LimInf,LimSup)
  

Punto 4. Afijacion de #Neyman
Nh = c(132,92,27)
sh = c(36,25,9)

wh=(Nh*sh)/(sum(Nh*sh)) #Afijacion de Neyman
n =300*wh #Distribucion de la muestra segun Neyman
ceiling(n)
n_neyman = sum(floor(n))

Punto 5: TOTAL poblacional.
Para el calculo del total, se necesita el promedio pues el total-estratificado es
promedio*Nh

Para ello, se tiene que:
  la mumestra es 30.
#Este codigo es impractico, usar el de DANIELA
datos_trabajadores=data.frame(scan(what=list(Obreros=0,Tecnicos=0,Administrativos=0)))
  8 4 1
  24 0 8
  0 8 0
  0 3 0
  16 1 0
  32 5 0
  6 24 0
  0 12 0
  16 2 0
  7 8 0
  4 0 0
  4 0 0
  9 0 0
  5 0 0
  8 0 0
  18 0 0
  2 0 0
  0 0 0
attach(datos_trabajadores)
  
 
Nh = c(132,92,27)
N = sum(Nh)
nh= c(18,10,2)
yh = c(sum(Obreros)/18,sum(Tecnicos)/10,sum(Administrativos)/2)
y_est = (1/N)*sum(Nh*yh)

mediaEstratificada <- function(Nh, yh){
  N <- sum(Nh)
  yest <- sum(Nh*yh)/N
  return(yest)
}

Total=mediaEstratificada(Nh,yh)*N

#Calculo varianza (Sigma cuadrado)
A = c(8,24,0,0,16,32,6,0,16,7,4,4,9,5,8,18,2,0)
B = c(4,0,8,3,1,5,24,12,2,8)
C= c(1,8)
sigma_2 = c(36,25,9)
s2h<-c(var(A),(var(B)),var(C))
varianzaMediasEnCadaEstrato <- function(Nh, nh, s2h){
  Varhaty <- (1- nh/Nh)*s2h/nh
  return(Varhaty)
}
nh= c(18,10,2)
Varianzas_Estratos_horas= varianzaMediasEnCadaEstrato(Nh,nh,sigma_2)

varianza_yestratificado=(1/N^2)*sum(Nh^2*Varianzas_Estratos_horas) #Varianza media

Varianza_Total_Poblacional = sum(N^2*varianza_yestratificado)

Finalmente, el limite del error B es tq

B = 2*sqrt(Varianza_Total_Poblacional)

Ahora, un IC de *Aproximadamente* 95% para Total poblacional esta dado por:
  
  LimInf= Total-B
  LimSup = Total +B
#El total esta dado entre (1227.097;2580.703)
  
  

  
##tama?o de la muestra aproximado para miu##
Punto 6:
  
  Nh=c(112,68,39)
  N=sum(Nh)
  B = 2*sqrt(0.1)
  D = B^2/4
  s2h=c(2.25,3.24,3.24) #Sigma cuadrado
  C = c(9,25,36) #Costo muestreo
  sh = sqrt(s2h)
  wh=((Nh*sh)/sqrt(C))/sum((Nh*sh)/sqrt(C)) #Afijacion Optima Por costos
  n=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)) #Tamaño de la muestra
  n_total= ceiling(n)
  n_estratos=ceiling(n*wh)
 
Punto 7:
  
  #Piden promedio
  A = c(80,92,68,85,72,87,85,91,90,81,62,79,61,83)
  B = c(85,82,48,75,53,73,65,78,49,69,72,81,53,59,68,52,71,61,59,42)
  C =c(42,32,36,31,65,29,43,19,53,14,61,31,42,30,39,32)
Nh = c(55,80,64)
nh=c(14,20,16)
n = sum(nh)
N = sum(Nh)
sh = c(sqrt(var(A)),sqrt(var(B)),sqrt(var(C)))
a) Establezca promedio itinerario 1. 
promedio_1 = mean(A)
VarProm_1 = (1-14/55)*var(A)/14
LEE1 = qt(0.975,13)*sqrt(sqrt(VarProm_1))
IC_A = c(promedio_1-LEE1,promedio_1+LEE1)

b) Diferencia entre 1 y 2:
promedio_2 = mean(B)
VarProm_2 = (1-20/80)*sqrt(var(B))/20
LEE2 = qt(0.975,13)*sqrt(VarProm_2)  
IC_B = c(promedio_2-LEE2,promedio_2+LEE2)
  
  Son 15 puntos mejores, que se reflejan en la comparativa de los datos.

c)  
wh=(Nh*sh)/(sum(Nh*sh)) #Afijacion de neyman
n=floor(wh*50)

D) COn un LLE de 4 y con afijacion proporcional, 
calcule tamallo n para el promedio

 D = 4^2/4
 wh = (Nh)/sum(Nh)
 s2h = c(var(A),var(B),var(C))
 n1=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)) #Tamaño muestral
 floor(n1)

D) Usando afijacion de Neyman...  
  sh = sqrt(s2h)
wh=(Nh*sh)/(sum(Nh*sh))
n2=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)) #Tamaño muestral
floor(n2)

Ambas son iguales, se requiere el mismo tamaño muestral para los resultados pedidos.

Punto 8:
  Nh= c(86,72,52,30)
  nh=c(14,12,9,5)
  est1= c(97,67,42,125,25,92,105,86,27,43,45,59,53,21)
  est2 = c(125,155,67,96,256,47,310,236,220,352,142,190)
  est3 = c(142,256,310,440,495,510,320,396,196)
  est4 = c(167,655,220,540,780)
a) Estime el total de acres plantados y un LEE

n = sum(nh)
yh = c(mean(est1),mean(est2),mean(est3),mean(est4)) #Vector con medias muestrales estratos
mediaEstratificada <- function(Nh, yh){
  N <- sum(Nh)
  yest <- sum(Nh*yh)/N
  return(yest)
} 
mu = mediaEstratificada(Nh,yh)
N =sum(Nh)
s2h = c(var(est1),var(est2),var(est3),var(est4)) 

total = N*mu
 #Calculo varianza muestral de la media en cd estrato

varianzaMediasEnCadaEstrato <- function(Nh, nh, s2h){
  Varhaty <- (1- nh/Nh)*s2h/nh
  return(Varhaty)
}
Varhaty=varianzaMediasEnCadaEstrato(Nh,nh,s2h) #Varianza y gorro

#Funcion estimadora varianza de la media

varianzaEstimadorMediaEstrarificada <- function(Nh, Varhaty){
  N <- sum(Nh)
  sum(Nh^2 * Varhaty)/(N^2)
}

Varianza_y_gorro = varianzaEstimadorMediaEstrarificada(Nh,Varhaty)
Varianza_t_gorro = sum(Varianza_y_gorro*N^2)
Con un LEE de una Z de aproximadamente 95% el IC pedido es

B = 2*sqrt(Varianza_t_gorro)

IC = c(total-B,total+B) 

b) Si el estudio es con un LEE de 5000 encuentre tamaño muestra con Neyman.
Con este dato, se esta fijando a B en 5 mil

n=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)) #Formula tamaño muestral.

Para ello, se necesita calcular 

sh = sqrt(s2h)

wh=(Nh*sh)/(sum(Nh*sh)) #Afijacion optima de neyman (costo igual)

D = 5000^2/(2^2*N^2)

Finalmente, se usa la formula del tamaño muestral n

n=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)) #Formula tamaño muestral.

Punto 9: 
N= 96
Nh = c(43,53) 
sigma1 = (20-5)/6 #varianza hombres
sigma2= (14-3)/6 #Varianza mujeres
sh = c(sigma1,sigma2)
s2h = sh^2
#Costos iguales
wh=(Nh*sh)/(sum(Nh*sh)) #Afijacion optima de neyman (costo igual)
B=1
D=B^2/2^2
n=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)) #Formula tamaño muestral.
ceiling(n)  

Punto 10: #Proporcion de quienes lo van a usar y quienes no las van a usar.
  Estrato 1: Personas ciudad que lo usan
Estrato 2: Personas ciudad que NO lo usan
Estrato 3: Personas alrededores lo usan
Estrato 4: Personas alrededores NO lo usan
C = c(4,8) #Costos de muestreo zona urbana y rural

# Se dice que el 90% de los usuarios actualoes lo va a usar
# Se dice que el 50% NO usuarios lo va a usar.

Usuarios_Estratos=c(97,43,145,68)

personas_urbanos=c(97,43)
personas_rural = c(145,68)

proporcion_urbana=97/sum(personas_urbanos) #Proporcion pers urban. Asisten
proporcion_rural=145/sum(personas_rural) #Proporcion, rural, no asisten

ph = c(proporcion_urbana,proporcion_rural)

qh = 1-ph

varianza = ph*qh

N = sum(Usuarios_Estratos)
P_est = 1/N*sum(Usuarios_Estratos*ph)
Varianza_Pest = 1/N^2*sum(Usuarios_Estratos^2*varianza)
raiz_varianza = sqrt(Varianza_Pest)

a) #Sea B = 0.05, halle tamaño n y afijacion necesaria

B = 0.05
D = B^2/(2^2) #usando normal. 

s2h = Varianza_Pest
sh = raiz_varianza

wh=((Nh*sh)/sqrt(C))/sum((Nh*sh)/sqrt(C))

n=(sum((Nh^2*s2h/wh)))/((N^2*D)+sum(Nh*s2h)) #Formula tamaño muestral.
ceiling(n)
  

b) 
ph = c(0.87,0.93,0.6,0.53)

Usuarios_Estratos=c(97,43,145,68) #Este es Nh

Usaran = Usuarios_Estratos*ph  

NoUsaran = Usuarios_Estratos-Usaran

N = sum(Usuarios_Estratos)
  
P_est = sum(ph*Usuarios_Estratos)/N

P_est

qh= 1-ph

varianza_ph = ph*qh

varianzaP_est= 1/N^2*sum(Usuarios_Estratos^2*varianza_ph)

IC = c(P_est-varianzaP_est,P_est+varianzaP_est)
IC

Punto 11: #PostEstratificacion
  
  A =c (110,142,212,227,167,130,194)
  B = c(387,345,465,308,280,480,355,405)
yh = c(mean(A),mean(B))
Nh = c(34,22)
N = sum(Nh)
nh = c(7,8)
n=sum(nh)
y_bar_post= 1/N*sum(Nh*yh) #Media muestral postestratificada
y_bar_post
s2h =c(var(A),var(B))
Var_ybar_post = ((N-n)/(N^2*n))*sum(Nh*s2h)+(1/n^2)*((N-n)/(N-1)*sum((1-Nh/N)*s2h))
Var_ybar_post
B = 2*sqrt(Var_ybar_post)

IC = c(y_bar_post-B,y_bar_post+B)
IC

Punto 12: 
Frecuencia = c(2,4,6,6,5,8,10,14,19,13,3,7)
raiz_f= sqrt(Frecuencia)
raiz_f  







