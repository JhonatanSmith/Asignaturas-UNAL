library(tidyverse)
library(magrittr)
library(forecast)
library(janitor)
library(lubridate)

C:\Users\Unalmed\Desktop\PRESENTACION18\petroleo_brent_historico.csv
C:\Users\Unalmed\Desktop\PRESENTACION18\trm_historico.csv

petroleo <- read.csv("C:/Users/Unalmed/Desktop/PRESENTACION18/petroleo_brent_historico.csv",
                     encoding = "utf-8", dec = ",")
trm <- read.csv("C:/Users/Unalmed/Desktop/PRESENTACION18/trm_historico.csv") # R ""

View(petroleo)
View(trm)

str(petroleo)
str(trm)

dim(petroleo)
dim(trm)

petroleo %<>% clean_names()

petroleo$i_fecha %<>% str_replace_all(.,"\\.","-") %>% as.Date(format = "%d-%m-%Y")
trm$VIGENCIADESDE %<>% as.Date(format = "%d/%m/%Y")


petroleo %<>% rename("fecha" = "i_fecha")

trm %<>% rename("fecha" = "VIGENCIADESDE")


bd_juntas <- merge(petroleo, trm, by = "fecha")
View(bd_juntas)

bd_juntas %<>% rename("brent" = "apertura", "trm" = "VALOR")

bd_juntas %<>% arrange(-desc(fecha))


bd_juntas %>% ggplot(aes(x = fecha, y = trm/sd(trm))) +
  geom_line() +
  geom_line(aes(x = fecha, y = brent/sd(brent)),col="red") +
  scale_y_continuous() # pensar

modelo1 <- lm(trm ~ brent, data = bd_juntas)

summary(modelo1)
acf(modelo1$residuals)
