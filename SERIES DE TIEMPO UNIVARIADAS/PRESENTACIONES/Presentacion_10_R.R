require(tidyverse)
require(magrittr)
require(readr)
require(janitor)
direccion <- "../../DATOS/incidentes_viales_motos.csv"
bd_incidentes <- read_delim(direccion,
                            delim = ";") 
bd_incidentes %<>% clean_names() 
bd_incidentes %>% dim()


bd_incidentes %>% head(n=3)


bd_incidentes %>% tail(n=3)


as.Date("27/01/15", format="%d/%m/%y") # y minúscula

as.Date("27/01/2015", format="%d/%m/%y") # y minúscula

as.Date("27/01/2015", format="%d/%m/%Y") # Y mayúscula

nchar("27/01/15") 
nchar("27/01/2015")


aux1 <- as.Date(rep(NA, nrow(bd_incidentes)))
aux2 <- bd_incidentes$fecha_incidente
ind1<-which(nchar(aux2)==8)
ind2<-which(nchar(aux2)==10)
aux1[ind1] <- as.Date(aux2[ind1], format = "%d/%m/%y")
aux1[ind2] <- as.Date(aux2[ind2], format = "%d/%m/%Y")
bd_incidentes$fecha_incidente<-aux1

require(lubridate)
bd_incidentes$mes<-month(bd_incidentes$fecha_incidente)
bd_incidentes$anio<-year(bd_incidentes$fecha_incidente)
resum1 <- bd_incidentes %>% group_by(anio, mes) %>% 
  summarise(accidentes=n())
resum1$fecha <- as.Date(
  paste(resum1$anio,resum1$mes,1,sep="-"),
  format = "%Y-%m-%d")

resum1 %>% ggplot(aes(x=fecha, y=accidentes))+
  geom_line()


acf(resum1$accidentes)

pacf(resum1$accidentes)

modelo3 <- arima(resum1$accidentes, order=c(6, 0, 0))
modelo3

modelo3$var.coef

round(modelo3$var.coef,4)

se <- modelo3$var.coef %>% diag() %>% sqrt() %>% round(4)

modelo3$coef-1.96*se

modelo3$coef+1.96*se