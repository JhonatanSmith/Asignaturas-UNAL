library(car)
library(perturb)
library(leaps)
library(olsrr)

###INGRESO DE DATOS POR TECLADO
###COL1="punt.Aptitud",COL2="Prueba1",COL3="Prueba2",COL4="Prueba3",COL5="Prueba4"
datos=data.frame(scan(what=list(punt.Aptitud=0,Prueba1=0,Prueba2=0,Prueba3=0,Prueba4=0)))
94 122 121 96 89
71 108 115 98 78
82 120 115 95 90
76 118 117 93 95
111 113 112 109 109
64 112 96 90 88
109 109 129 102 108
104 112 119 106 105
80 115 101 95 88
73 111 95 95 84
127 119 118 107 110
88 112 110 100 87
99 120 89 105 97
80 117 118 99 100
99 109 125 108 95
116 116 122 116 102
100 104 83 100 102
96 110 101 103 103
126 117 120 113 108
58 120 77 80 74

attach(datos)

cor(datos)

modelo=lm(punt.Aptitud~Prueba1+Prueba2+Prueba3+Prueba4)
summary(modelo)
vif(modelo)
colldiag(modelo)
ols_coll_diag(modelo)


linearHypothesis(modelo,c("Prueba1=0","Prueba2=0"))

#con datos centrados
Ind=colldiag(modelo,center=TRUE)
X=model.matrix(modelo)[,-1]
val.prop=prcomp(X,center=TRUE,scale=TRUE)$sdev^2
resul=data.frame(Val.propio=val.prop,Ind.Cond=Ind$condindx,Pi=Ind$pi)
resul


#Selección backward
ols_step_backward_p(modelo,prem=0.05) #tabla resumen
ols_step_backward_p(modelo,prem=0.05)$model #Estimaciones en modelo final
ols_step_backward_p(modelo,prem=0.05,details = T) #Impresión detallada de cada paso
ols_step_backward_p(modelo,prem=0.05,progress = T) #Impresión detallando resultados con mejor modelo


#Selección forward
ols_step_forward_p(modelo,penter=0.05) #tabla resumen
ols_step_forward_p(modelo,penter=0.05)$model #Estimaciones en modelo final
ols_step_forward_p(modelo,penter=0.05,details = T) #Impresión detallada de cada paso
ols_step_forward_p(modelo,penter=0.05,progress = T) #Impresión detallando resultados con mejor modelo

#Selección stepwise
ols_step_both_p(modelo,pent = 0.05, prem = 0.05) #tabla resumen
ols_step_both_p(modelo,pent = 0.05, prem = 0.05)$model #Estimaciones en modelo final
ols_step_both_p(modelo,pent = 0.05, prem = 0.05,details=T) #Impresión detallada de cada paso
ols_step_both_p(modelo,pent = 0.05, prem = 0.05,progress=T) #Impresión detallando resultados con mejor modelo



#Todas las regresiones posibles; da información del Cp, R2, R2adj
k=ols_step_all_possible(modelo) #guardando tabla de toda las regresiones posibles
                                #con medidas Cp, R2, R2adj
k 
#Gráficas para identificar meejor modelo según criterios de selección
plot(k)

ols_press(modelo) #Calcula PRESSp solo para modelo completo

