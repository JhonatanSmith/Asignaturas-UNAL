library(tidyverse)
library(magrittr)
library(forecast)
library(janitor)
library(lubridate)
library(lmtest)

# C:\Users\Unalmed\Desktop\PRESENTACION18\petroleo_brent_historico.csv
# C:\Users\Unalmed\Desktop\PRESENTACION18\trm_historico.csv

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


bd2 = round(c(2,4,6)*sd(bd_juntas$brent), 0)


bd_juntas %>% ggplot(aes(x = fecha, y = trm/sd(trm))) +
  geom_line() +
  geom_line(aes(x = fecha, y = brent/sd(brent)),col="red") +
  scale_y_continuous(name = "precio brent", labels = bd2,
                     breaks = c(2,4,6),
                     sec.axis = sec_axis(~.*sd(bd_juntas$trm), name = "trm")
                     ) # pensar

modelo1 <- lm(trm ~ brent, data = bd_juntas)
checkresiduals(modelo1$residuals)

summary(modelo1)
acf(modelo1$residuals)

matriz_disenio <- model.matrix(~bd_juntas$brent)[,-1]
matriz_disenio


modelo2 <- auto.arima(bd_juntas$trm, xreg = matriz_disenio,
                      stepwise = F, approximation = F)


modelo2
checkresiduals(modelo2)
coeftest(modelo2)

qqnorm(modelo2$residuals)
qqline(modelo2$residuals)


dim(bd_juntas)

bd_juntas$anio <- year(bd_juntas$fecha)

table(bd_juntas$anio)

train = bd_juntas %>% filter(anio < 2014)
test = bd_juntas %>% filter(anio >= 2014)

disenio3 <- model.matrix(~-1+train$brent)
colnames(disenio3) <- "brent"
modelo3 <- auto.arima(train$trm, xreg = disenio3,
                      stepwise = F, approximation = F)
modelo3

require(forecast)
disenio3_test <- model.matrix(~-1+test$brent)
colnames(disenio3_test) <- "brent"
fore3 <- forecast(modelo3, xreg = disenio3_test, h = nrow(test))

dim(train)
