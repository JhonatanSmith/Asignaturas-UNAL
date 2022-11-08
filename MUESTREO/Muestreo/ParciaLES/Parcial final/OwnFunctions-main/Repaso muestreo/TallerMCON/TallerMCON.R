#Taller4MConglomerados
#EJERCICIO 1: SE DISCUTE EN GRUPO
library(tidyverse)
#EJERCICIO 2
N <- 96
sierras <- read.table("Sierras.txt", header = T)

Mi <- sierras$Numero_de_sierras; n <- length(Mi)
taui <- sierras$Costo_total

(res <- tau_muc_mu_r(Mi, taui, N))
#Se usan estimadores insesgados
#Literal (a)
#mu = 19.73077; B = 5.08825

#Literal (b)
#Tau = 12312; B = 3175.06787 approx = 3175

#Literal (c): Si es un total, la respuesta es identica a (b)
#Para promedio: Mu = 19.73077; B = 5.08825
Mo <- 710
tau_muc_mu(Mi, taui, N, Mo)
#Mu = 17.34085; B = 4.471927

#Literal (d)
B <- 2; D <- (B * (Mo/N) * 1/2)^2
(n <- sample_size(N, res$S2con, D)) #aprox 94

rm(list = setdiff(ls(), lsf.str()))

##########################################################
#EJERCICIO 3
col <- read.table("est_col.txt") %>%
  arrange(V1)
colnames(col) <- c("Aula", "Num_Est", "Calificacion")

N <- 108; n <- 25
Mi <- col$Num_Est
tau_i <- col$Calificacion

#Literal (a)
res <- tau_muc_mu_r(Mi, tau_i, N, mean(Mi) * N)

#Mu = 51.559; B = 5.101166

#Literal (b): 
mbar <- mean(Mi)
N <- 100; B <- 2; D <- (B * mbar * 1/2)^2; S2 <- res$S2con
sample_size(N, S2, D)
#n = 68

rm(list = setdiff(ls(), lsf.str()))

############################################################
#EJERCICIO 4
#Machetazo (de momento)
jub <- read.table("jubilados.txt", header = T)
Mi <- jub$Num_Emp; mo <- mean(Mi)*N
N <- 87; n <- 15
Ai <- jub$Apoyan
#Literal (a)
res <- EstimacionesPA(Ai, Mi, N, n, mo)

#Literal (b)
B <- 0.08; D <- (B * mean(Mi) * 1/2)^2; pcon <- res$Estimaciones[, 1]
sample_size(N, res$S2pcon, D)

rm(list = setdiff(ls(), lsf.str()))

#############################################################
#EJERCICIO 5
hogares <- read.table("gasto_hogares.txt") %>%
  arrange(V1)

colnames(hogares) <- c("Barrio", "Num_Hog", "Gasto_Total")

#Literal (a)
N <- 60; n <- 20
Mi <- hogares$Num_Hog
gasto <- hogares$Gasto_Total
res <- tau_muc_mu_r(Mi, gasto, N); S2 <- res$S2con
#Mu = 40.16884; B = 1.772288

#Literal (b)
#tau = 157020; B = 6927.875044
#Para totales con el tamaño de conglomerados distintos
#D = (B/2N)^2
#Literal (c)
B <- 5000; D <- (B/(2* N))^2
sample_size(N, S2, D)

rm(list = setdiff(ls(), lsf.str()))

#######################################################################
#EJERCICIO 6
p1 <- c(16.1, 15.9, 16.1, 16.2, 15.9, 15.8, 16.1, 16.2, 16.0, 15.9, 15.8, 
        16.0)
p2 <- c(15.9, 16.2, 15.8, 16.0, 16.3, 16.1, 15.8, 15.9, 16.0, 16.1, 16.1, 
        15.9)
p3 <- c(16.2, 16.0, 15.7, 16.3, 15.8, 16.0, 15.9, 16.0, 16.1, 16.0, 15.9, 
        16.1)
p4 <- c(15.9, 16.1, 16.2, 16.1, 16.1, 16.3, 15.9, 16.1, 15.9, 15.9, 16.0, 
        16.0)
p5 <- c(16.0, 15.8, 16.3, 15.7, 16.1, 15.9, 16.0, 16.1, 15.8, 16.0, 16.1, 
        15.9)

datos <- rbind(p1, p2, p3, p4, p5)
taui <- apply(datos, 1, sum)
M <- 12; n <- 5

#Estimaciones puntuales
muc_hat <- 1/n * sum(taui)
mu_hat <- muc_hat/M

#S2con
S2con <- (1/(n - 1)) * sum((taui - muc_hat)^2)

#LEE
var_muc_hat <- (S2con/n)
var_mu_hat <- (1/M^2) * var_muc_hat
B <- 2 * sqrt(var_mu_hat)

rm(list = setdiff(ls(), lsf.str()))

######################################################################
#EJERCICIO 7
votantes <- read.table("votantes.txt")
colnames(votantes) <- c("Num_vot", "Favor_A")

Ai <- votantes$Favor_A
Mi <- votantes$Num_vot
n <- 50; N <- 497

#Literal (a)
(res <- EstimacionesPA(Ai, Mi, N, n))
#pcon = 0.5734364; B = 0.03182591

#Literal (b)
B <- 0.05; D <- (B * mean(Mi)/2)^2; S2 <- res$S2pcon
sample_size(N, S2, D)
#n = 21

rm(list = setdiff(ls(), lsf.str()))

#######################################################################
#EJERCICIO 8
guard_bosques <- read.table("guardabosques.txt")
colnames(guard_bosques) <- c("Num_arboles", "Altura_promedio")

n <- 20; N <- 386
Mi <- guard_bosques$Num_arboles
taui <- Mi * guard_bosques$Altura_promedio
res <- tau_muc_mu_r(Mi, taui, N)
#Mu = 5.908987e+00; B = 0.4920488

rm(list = setdiff(ls(), lsf.str()))

#######################################################################
#EJERCICIO 9
N <- 175; M <- 4; n <- 25
Ai <- c(2, 4, 0, 1, 2, 0, 4, 1, 3, 1, 2, 0, 1, 1, 2, 2, 4, 1, 0, 0,
        3, 1, 2, 2, 1)
prop_tot_con(M, N, n, Ai = Ai)
EstimacionesPA(Ai, M_i = rep(4, length(Ai)), N = N, n = n)
#p_con = 0.4; B_p = 0.1164965

rm(list = setdiff(ls(), lsf.str()))

#######################################################################
#EJERCICIO 10
bd <- read.table("bdp10.txt")
colnames(bd) <- c("Num_Elementos", "Total_dolares")

N <- 48; n <- 10
taui <- bd$Total_dolares
Mi <- bd$Num_Elementos
res <- tau_muc_mu_r(Mi, taui, N)
#tau = 3532.8; B = 539.5013994

rm(list = setdiff(ls(), lsf.str()))

#######################################################################
#EJERCICIO 11
bd11 <- list(florida = data.frame(Mi = c(12,20,8,14,24,15,10,6),
                                  yi = c(40,52,30,36,71,48,39,21)), 
             cali = data.frame(Mi = c(16,8,4,3,12,17,24,30,21,9),
                               yi = c(51,32,11,10,33,39,61,37,40,41)))


#######################################################################
#EJERCICIO 12
man_hog <- read.table("manzanas_hogares.txt", header = T)
n <- 8

#Literal (a)
Mi_hog <- man_hog$Num_hogares
tot_res <- man_hog$Num_resid
mbar_hog <- mean(Mi_hog)

#Residentes por hogar
mu_resid <- sum(tot_res)/sum(Mi)
S2_res <- (tot_res - Mi_hog*mu_resid)^2
var_mu_resid <- (1/mbar_hog)^2 * sum(S2_res)/((n - 1)*n)
B_res <- 2 * sqrt(var_mu_resid)  
  
#Literal (b)
#Residentes por habitacion
Mi_hab <- man_hog$Num_hab
mbar_hab <- mean(Mi_hab)
mu_hab <- sum(tot_res)/sum(Mi_hab)
S2_hab <- (tot_res - Mi_hab*mu_hab)^2
var_mu_hab <- (1/mbar_hab)^2 * sum(S2_hab)/(n*(n-1))
B_hab <- 2 * sqrt(var_mu_hab)

rm(list = setdiff(ls(), lsf.str()))

#######################################################################
#EJERCICIO 13
bd13 <- c(2, 0, 1, 3, 2, 0, 0, 1, 3, 4)
M <- 12; n <- 10
Pi <- bd13/12

#Literal (a)
prop_tot_con(M, Inf, n, Pi)
#p_con = 0.13333333; B = 0.07535922

#Literal (b)
prop_tot_con(M, 50, n, Pi)
#p_con = 0.13333333; B = 0.06740334

rm(list = setdiff(ls(), lsf.str()))

########################################################################
#EJERCICIO 14
#ES PURA CARRETA XD

########################################################################
#EJERCICIO 15
N.plantones <- c(52,56,60,46,49,51,50,61,60,45)
clusters <- list(c(12, 11 , 12, 10, 13), c(10, 9, 7, 9, 8, 10),
                 c(6, 5, 7, 5, 6, 4), c(7, 8, 7, 7, 6), c(10, 11, 13, 12, 12),
                 c(14, 15, 13, 12, 13),c(6, 7, 6, 8, 7), c(9, 10, 8, 9, 9, 10),
                 c(7, 10, 8, 9, 9, 10), c(12, 11, 12, 13, 12, 12))
needed <- first(clusters, N.plantones)
needed
EstRazMu(N.plantones, 50, 10, needed$mi, needed$Si2, needed$taui)
#Mu_r2 = 9.378931; B = 1.454569

#########################################################################
#EJERCICIO 16
#Se hace con info del 15
tau_mu_2(needed$taui, needed$Si2, N.plantones, needed$mi, 50, 2600)

#mu(2) = 9.559295; B = 1.367188

rm(list = setdiff(ls(), lsf.str()))

#########################################################################
#EJERCICIO 17
depto <- read.table("departamentos.txt")
colnames(depto) <- c("Depto", "Secretarias", "Secretarias_muestreadas",
                     "Media", "Varianza_muestral")
Mi <- depto$Secretarias; mi <- depto$Secretarias_muestreadas
N <- 12; yi_bar <- depto$Media; si2 <- depto$Varianza_muestral
n <- 4
EstRazMu(Mi, N, n, mi, si2, yibar = yi_bar)

#Tau_r2 = 3980.7; B =183.4348

rm(list = setdiff(ls(), lsf.str()))

#######################################################################
#EJERCICIO 18
bd <- read.table("bdp18.txt")
colnames(bd) <- c("Ciudad", "Supermercados", "Supermercados_muestreados", 
                  "Media", "Varianza_muestral")
N <- 20; n <- 5
Mi <- bd$Supermercados; mi <- bd$Supermercados_muestreados
yi_bar <- bd$Media; si2 <- bd$Varianza_muestral
EstRazMu(Mi, N, n, mi, si2, yibar = yi_bar)

#Mu_r2 = 97.97279; B = 10.99553

rm(list = setdiff(ls(), lsf.str()))














